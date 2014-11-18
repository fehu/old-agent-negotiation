package feh.tec.agents.light

import java.util.UUID

import feh.tec.agents.light.Message.ProposalId
import feh.tec.agents.light.impl.agent.create
import feh.util._
import feh.tec.agents.light.spec.RequiresDistinctPriority

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

object QueenSpec{
  def apply(acceptanceCheckDelay: FiniteDuration) = new QueenSpec(acceptanceCheckDelay)

  case class FallbackRequest(negotiation: NegotiationId, priority: Priority, id: UUID)(implicit val sender: AgentRef) extends Message.HasPriority{
    def asString: String = s"I have no more values to propose in $negotiation ($priority)"
  }
  case class IWillMove(negotiation: NegotiationId, priority: Priority, respondingTo: UUID)(implicit val sender: AgentRef) extends Message.HasPriority{
    def asString: String = s"I will move in $negotiation as you asked ($priority)"
  }

//  case class CheckAcceptance(neg: NegotiationId, pid: ProposalId) extends UserMessage
  
  case object FallbackState extends NegotiationState 
}

class QueenSpec(val acceptanceCheckDelay: FiniteDuration) extends create.PPI.AllVarsSpec with RequiresDistinctPriority{
  import QueenSpec._

  val priorities = mutable.HashMap.empty[NegotiationId, mutable.HashMap[AgentRef, Option[Priority]]]

  def allPrioritiesKnown(neg: NegotiationId) = priorities(neg).forall(_._2.isDefined)
  def maxPriority(neg: NegotiationId)(implicit ag: create.PPI.Ag) =
    (priorities(neg) + (ag.ref -> ag.get(neg).currentPriority.raw))
      .mapValues(_.get).maxBy(_._2.get)

  private val proposalAcceptance = mutable.HashMap.empty[NegotiationId, (ProposalId, mutable.HashMap[AgentRef, Option[Boolean]])]

  def setProposalAcceptance(msg: Message.ProposalResponse, v: Boolean)(implicit ag: create.PPI.Ag) = {
    val (pid, map) = proposalAcceptance
      .getOrElseUpdate(msg.negotiation, msg.respondingTo -> mutable.HashMap(ag.get(msg.negotiation).scope().zipMap(_ => Option.empty[Boolean]).toSeq: _*))
    assert(pid == msg.respondingTo)
    map(msg.sender) = Option(v)
  }
  def clearProposalAcceptance(neg: NegotiationId)(implicit ag: create.PPI.Ag) = {
    ag.log.debug("clearProposalAcceptance + " + proposalAcceptance)
    proposalAcceptance -= neg
  }
  
//  def scheduleAcceptanceCheck(msg: Message.ProposalResponse)(implicit ag: create.PPI.Ag) = ag.context.system.scheduler
//    .scheduleOnce(acceptanceCheckDelay, ag.self, CheckAcceptance(msg.negotiation, msg.respondingTo))(ag.context.dispatcher)
  
  def allAccepted(neg: NegotiationId, pid: ProposalId) =
    proposalAcceptance.get(neg).exists{
      case (id, map) => id == pid  && map.forall(_._2.getOrElse(false))
    } //.exists(_._2.forall(_._2.exists(identity)))

  def whenProposalAccepted(msg: Message.ProposalResponse)(implicit ag: create.PPI.Ag): Unit = whenProposalAccepted(msg.negotiation, msg.respondingTo)
  def whenProposalAccepted(neg: NegotiationId, pid: ProposalId)(implicit ag: create.PPI.Ag): Unit = if(allAccepted(neg, pid)){
    ag.log.info("all accepted, setting state to **Waiting** " + proposalAcceptance(neg))
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
  
//  processUserMessage <:= {
//    implicit ag => {
//      case CheckAcceptance(neg, pid) => whenProposalAccepted(neg, pid)
//    }
//  }

  def onFallbackRequest(req: FallbackRequest)(implicit ag: create.PPI.Ag): Unit = req match {
    case req@FallbackRequest(negId, pr, id) =>
      import ag._
      val neg = get(negId)
      if(neg.currentPriority().get == pr.get + 1) {
        if(neg.currentState() == NegotiationState.Waiting) neg.currentState update NegotiationState.Negotiating
        ag.setNextProposal(negId)
        val resp = IWillMove(negId, neg.currentPriority(), id)
        req.sender ! resp
        sendToAll(neg.currentProposal())
      }
  }
  
  def nothingToProposeIfHasMaxPriority(negId: NegotiationId)(implicit ag: create.PPI.Ag): Option[Unit] =
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
  
  initialize after {
    ag => _ =>
//      ag.negotiations.foreach(_.currentPriority update new Priority(1))
      ag.log.info("initialized")
  }
  start andThen {
    ag =>
      overridden =>
        import ag._
        overridden(ag)
        negotiations.foreach {
          neg =>
            if(neg.currentIterator.raw.isEmpty) neg.currentIterator update ag.newIterator(neg.id)
            log.info("Iterating the domain with " + neg.currentIterator())
            ag.setNextProposal(neg.id)
            //            ag.nextValues()
            //            val proposal = neg.currentProposal.getOrElse {
            //              Message.Proposal(Message.ProposalId.rand, neg.id, neg.currentPriority(), neg.currentValues())
            //            }
            neg.currentState update NegotiationState.Negotiating
            log.info(s"negotiation ${neg.id} started, scope = ${neg.scope()}")
            sendToAll(neg.currentProposal())
        }
  }

  def respondToProposal(msg: Message.Proposal)(implicit ag: create.PPI.Ag) = {
    import ag._
    msg match{
      case Message.Proposal(propId, negId, _, values) =>
        val response =
          if(msg.satisfiesConstraints) Message.Accepted(negId, propId, get(negId).currentPriority(), get(negId).currentValues())
          else Message.Rejected(negId, propId, get(negId).currentPriority(), get(negId).currentValues())
//        priorities += msg.sender -> msg.priority
        respond(response)
    }
  }

  onProposal <:= {
    implicit ag =>
      import ag._
      {
        case msg if(myPriority isLowerThenOf  msg ) && hasState(msg, NegotiationState.Waiting) =>
          if(!msg.satisfiesConstraints) {
            get(msg.negotiation).currentState update NegotiationState.Negotiating
            sendToAll(ag.setNextProposal(msg.negotiation))
          }
          respondToProposal(msg)
        case msg if myPriority isLowerThenOf  msg => respondToProposal(msg)
        case msg if myPriority isHigherThenOf msg => respondToProposal(msg)
      }
  }

  onRejection <:= {
    implicit ag => {
      case msg if msg.respondingTo != ag.get(msg.negotiation).currentProposal().id => //ignore
      case msg if ag.myPriority isHigherThenOf msg =>
        //mark as accepted
        setProposalAcceptance(msg, v = true)
        whenProposalAccepted(msg)
        ag.log.debug("rejected: myPriority isHigherThenOf " + msg)
      case msg if ag.myPriority isLowerThenOf  msg =>
        setProposalAcceptance(msg, v = false)
        whenProposalAccepted(msg)
        ag.log.debug("!!!!!!!!!!!!!! rejected")
        val prop = ag.setNextProposal(msg.negotiation)
        ag.log.debug("rejected: spam new proposal " + prop)
        ag.sendToAll(prop)
    }
  }

  updateCurrentProposal /*setNextProposal*/ andThen {
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
        ag.log.debug("accepted: " + msg)
        setProposalAcceptance(msg, v = true)
        whenProposalAccepted(msg)
    }
  }

}