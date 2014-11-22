package feh.tec.agents.lite

import java.util.UUID

import feh.tec.agents.lite.Message.ProposalId
import feh.tec.agents.lite.impl.FailedConfigurationsChecks
import feh.tec.agents.lite.impl.agent.{FailureChecks, create}
import feh.util._
import feh.tec.agents.lite.spec.RequiresDistinctPriority
import scala.collection.mutable
import scala.reflect.ClassTag

object QueenSpec{
  def apply() = new QueenSpec

  case class FallbackRequest(negotiation: NegotiationId, priority: Priority, id: UUID)(implicit val sender: AgentRef) extends Message.HasPriority{
    def asString: String = s"I have no more values to propose in $negotiation ($priority)"
  }
  case class IWillMove(negotiation: NegotiationId, priority: Priority, respondingTo: UUID)(implicit val sender: AgentRef) extends Message.HasPriority{
    def asString: String = s"I will move in $negotiation as you asked ($priority)"
  }

//  case class CheckAcceptance(neg: NegotiationId, pid: ProposalId) extends UserMessage
  
  case object FallbackState extends NegotiationState

  type Agent = create.PPI.Ag with FailedConfigurationsChecks[create.PPI.Lang]
}

import QueenSpec._

class QueenSpec(implicit val agentTag: ClassTag[Agent]) extends create.PPI.AllVarsSpec[Agent]
  with RequiresDistinctPriority
{

  val priorities = mutable.HashMap.empty[NegotiationId, mutable.HashMap[AgentRef, Option[Priority]]]

  def allPrioritiesKnown(neg: NegotiationId) = priorities(neg).forall(_._2.isDefined)
  def maxPriority(neg: NegotiationId)(implicit ag: Agent) =
    (priorities(neg) + (ag.ref -> ag.get(neg).currentPriority.raw))
      .mapValues(_.get).maxBy(_._2.get)

  private val proposalAcceptance = mutable.HashMap.empty[NegotiationId, (ProposalId, mutable.HashMap[AgentRef, Option[(Message.ProposalResponse, Boolean)]])]

  def setProposalAcceptance(msg: Message.ProposalResponse, v: Boolean)(implicit ag: Agent) = {
    val (pid, map) = proposalAcceptance
      .getOrElseUpdate(msg.negotiation, msg.respondingTo -> mutable.HashMap(ag.get(msg.negotiation).scope().zipMap(_ => Option.empty[(Message.ProposalResponse, Boolean)]).toSeq: _*))
    assert(pid == msg.respondingTo)
    map(msg.sender) = Option(msg -> v)
  }

  def knownConfiguration(neg: NegotiationId, prop: ProposalId)(implicit ag: Agent): PartialValuesConfiguration = {
    val responses = proposalAcceptance.get(neg).filter(_._1 == prop).map(_._2.values.flatten.toList).getOrElse(Nil)
    val n = ag.get(neg)
    val myConf = n.currentPriority() -> n.currentValues()
    val confs = responses.map{case (resp, _) => resp.priority -> resp.myValues}.toMap + myConf
    new PartialValuesConfiguration(confs, neg)
  }


  def clearProposalAcceptance(neg: NegotiationId)(implicit ag: Agent) = proposalAcceptance -= neg

  def allAccepted(neg: NegotiationId, pid: ProposalId) =
    proposalAcceptance.get(neg).exists{
      case (id, map) => id == pid  && map.nonEmpty && map.forall(_._2.exists(_._2))
    }

  def whenProposalAccepted(msg: Message.ProposalResponse)(implicit ag: Agent): Unit = whenProposalAccepted(msg.negotiation, msg.respondingTo)
  def whenProposalAccepted(neg: NegotiationId, pid: ProposalId)(implicit ag: Agent): Unit = if(allAccepted(neg, pid)){
//    ag.log.info("all accepted, setting state to **Waiting** " + proposalAcceptance(neg))
    ag.get(neg).currentState update NegotiationState.Waiting
  }

  moreProcess <:= {
    implicit ag => {
      case msg: FallbackRequest if ag.hasState(msg, NegotiationState.Negotiating, NegotiationState.Waiting) => onFallbackRequest(msg)
      /* Fallback State */
      case msg: IWillMove if ag.hasState(msg, FallbackState) =>
        val neg = ag.get(msg.negotiation)
        neg.currentIterator update ag.newIterator(neg.id)
        ag.setNextProposal(neg.id)
        neg.currentState update NegotiationState.Negotiating
        ag.resendDelayedMessages()
        ag.sendToAll(neg.currentProposal())
      case msg: FallbackRequest if ag.hasState(msg, FallbackState) => ag.guardDelayedMessage(msg)
      case msg: Message.Proposal if ag.hasState(msg, FallbackState) => ag.guardDelayedMessage(msg)
      case msg if ag.hasState(msg, FallbackState) => // do nothing
    }
  }
  
  def onFallbackRequest(req: FallbackRequest)(implicit ag: Agent): Unit = req match {
    case req@FallbackRequest(negId, pr, id) =>
      import ag._
      val neg = get(negId)
      val myPr = neg.currentPriority()
      if(myPr.get == pr.get + 1) {
        if(neg.currentState() == NegotiationState.Waiting) neg.currentState update NegotiationState.Negotiating
        val failed = knownConfiguration(negId, neg.currentProposal().id).filter((p, _) => p >= myPr)
        val requiredSize = neg.scope().size - myPr + 1
        if(failed.size == requiredSize) {
          ag.guardFailedConfiguration(failed)
          sendToAll(FailureChecks.GuardFailedConfiguration(failed, myPr))
        }
        ag.setNextProposal(negId)
        val resp = IWillMove(negId, myPr, id)
        req.sender ! resp
        sendToAll(neg.currentProposal())
      }
  }
  
  def nothingToProposeIfHasMaxPriority(negId: NegotiationId)(implicit ag: Agent): Option[Unit] =
    if(maxPriority(negId)._1 == ag.ref) Some{
      sys.error("Failed to resolve negotiation")
    } else None

  beforeEachMessage andThen {
    ag => overridden => msg =>
      overridden(ag)(msg)
      priorities
        .getOrElseUpdate(
          msg.negotiation, mutable.HashMap(ag.get(msg.negotiation).scope().toSeq.zipMap(_ => Option.empty[Priority]): _*)
      ) += msg.sender -> Some(msg.priority)
  }

  nothingToPropose <:= {
    implicit ag => {
      negId =>
        clearProposalAcceptance(negId)(ag)
        nothingToProposeIfHasMaxPriority(negId).getOrElse{
          val neg = ag.get(negId)
          neg.currentState update FallbackState
          val req = FallbackRequest(negId, neg.currentPriority(), UUID.randomUUID())(ag.ref)
          ag.sendToAll(req)  
        }
    }
  }

  initialize before {
    ag =>
      ag.Reporting.Messages = false
      ag.Reporting.States = false
  }

  initialize after {
    ag => _ => ag.log.info("initialized")
  }
  start andThen {
    ag =>
      overridden =>
        import ag._
        overridden(ag)
        negotiations.foreach {
          neg =>
            if(neg.currentIterator.raw.isEmpty) neg.currentIterator update ag.newIterator(neg.id)
            val prop = ag.setNextProposal(neg.id)
            neg.currentState update NegotiationState.Negotiating
            sendToAll(prop)
        }
  }

  def respondToProposal(msg: Message.Proposal)(implicit ag: Agent) = {
    import ag._
    msg match{
      case Message.Proposal(propId, negId, _, values) =>
        val neg = get(negId)
        val response =
          if(msg.satisfiesConstraints)
            Message.Accepted(negId, propId, neg.currentPriority(), neg.currentValues(), neg.currentState() == NegotiationState.Waiting)
          else Message.Rejected(negId, propId, neg.currentPriority(), get(negId).currentValues())
        respond(response)
    }
  }

  onProposal <:= {
    implicit ag =>
      import ag._
      {
        case msg if(myPriority isLowerThenOf  msg) && !hasState(msg, FallbackState) =>
          if(!msg.satisfiesConstraints) {
            get(msg.negotiation).currentState update NegotiationState.Negotiating
            sendToAll(ag.setNextProposal(msg.negotiation))
          }
          respondToProposal(msg)
        case msg if myPriority isLowerThenOf  msg => respondToProposal(msg)
        case msg if myPriority isHigherThenOf msg => respondToProposal(msg)
      }
  }

  def rejected(neg: NegotiationId)(implicit ag: Agent) = {
    ag.sendToAll(ag.setNextProposal(neg))
  }

  onRejection <:= {
    implicit ag => {
      case msg if msg.respondingTo != ag.get(msg.negotiation).currentProposal().id => //ignore
      case msg if ag.myPriority isHigherThenOf msg =>
        //mark as accepted
        setProposalAcceptance(msg, v = true)
        whenProposalAccepted(msg)
      case msg if ag.myPriority isLowerThenOf  msg =>
        setProposalAcceptance(msg, v = false)
        rejected(msg.negotiation)
    }
  }

  updateCurrentProposal andThen {
    ag =>
      overridden =>
        negId =>
          clearProposalAcceptance(negId)(ag)
          overridden(ag)(negId)
  }

  onAcceptance <:= {
    implicit ag => {
      case msg if msg.respondingTo != ag.get(msg.negotiation).currentProposal().id => //ignore
      case msg =>
        ag.repeatingAFailure(msg) match {
          case Some(true) =>
//            ag.log.info("rejecting repeating error" + knownConfiguration(msg.negotiation, msg.respondingTo))
            rejected(msg.negotiation)
          case Some(false) | None =>
            setProposalAcceptance(msg, v = true)
            whenProposalAccepted(msg)
//          case None => // wait for more
        }
    }
  }

}