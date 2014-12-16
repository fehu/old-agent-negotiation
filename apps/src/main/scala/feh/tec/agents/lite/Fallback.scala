package feh.tec.agents.lite

import java.util.UUID
import feh.tec.agents.lite.Message.ProposalId
import feh.tec.agents.lite.impl.PriorityAndProposalBasedAgent
import feh.tec.agents.lite.impl.agent.FailureChecks
import feh.tec.agents.lite.impl.agent.create.SpecExt
import feh.tec.agents.lite.impl.spec.PriorityAndProposalBasedAgentSpec
import feh.tec.agents.lite.spec.RequiresDistinctPriority
import feh.util._

object Fallback {
  type Agent[Lang <: Language.ProposalBased with Language.HasPriority] =
    PriorityAndProposalBasedAgent[Lang] with ProposalEngine.Iterating[Lang] with FailedConfigurationsChecks[Lang]
      {
        type Negotiation <: Negotiation.HasProposal[Lang] with Negotiation.HasPriority with Negotiation.HasIterator with Negotiation.ChangingIssues
      }

  case class FallbackRequest(negotiation: NegotiationId, priority: Priority, id: UUID)(implicit val sender: AgentRef) extends Message.HasPriority{
    def asString: String = s"I have no more values to propose in $negotiation ($priority)"
  }
  case class IWillMove(negotiation: NegotiationId, priority: Priority, respondingTo: UUID)(implicit val sender: AgentRef) extends Message.HasPriority{
    def asString: String = s"I will move in $negotiation as you asked ($priority)"
  }

  case object FallbackState extends NegotiationState
}

trait FallbackSpec[Ag <: Fallback.Agent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]
  extends PrioritiesRegister[Ag, Lang] with ProposalAcceptanceRegister[Ag, Lang]
{
  self: PriorityAndProposalBasedAgentSpec[Ag, Lang] with RequiresDistinctPriority with SpecExt[Ag] =>

  import Fallback._

  def knownConfiguration(neg: NegotiationId, prop: ProposalId)(implicit ag: Ag): PartialValuesConfiguration = {
    val responses = proposalAcceptance.get(neg).filter(_._1 == prop).map(_._2.values.flatten.toList).getOrElse(Nil)
    val n = ag.get(neg)
    val myConf = n.currentPriority() -> n.currentValues()
    val confs = responses.map{case (resp, _) => resp.priority -> resp.myValues}.toMap + myConf
    new PartialValuesConfiguration(confs, neg)
  }


  def onFallbackRequest(req: FallbackRequest)(implicit ag: Ag): Unit = req match {
    case req@FallbackRequest(negId, pr, reqId) =>
      import ag._
      val neg = get(negId)
      val myPr = neg.currentPriority()

      if(myPr.get == pr.get + 1) {
        val failed = knownConfiguration(negId, neg.currentProposal().id).filter((p, _) => p >= myPr)
        val requiredSize = neg.scope().size - myPr + 1
        if(failed.size == requiredSize) {
          ag.guardFailedConfiguration(failed)
          sendToAll(FailureChecks.GuardFailedConfiguration(failed, myPr))
        }
        if(neg.currentState() == NegotiationState.Waiting) {
          neg.currentState update NegotiationState.Negotiating
          ag.setNextProposal(negId)
        }
        respondIWillMove(req, myPr)
        sendToAll(neg.currentProposal())
      }
  }

  protected def respondIWillMove(req: FallbackRequest, minePriority: Priority)(implicit ag: Ag) = {
    import ag._
    req.sender ! IWillMove(req.negotiation, minePriority, req.id)
  }

  def whenProposalAccepted(msg: Message.ProposalResponse)(implicit ag: Ag): Unit = whenProposalAccepted(msg.negotiation, msg.respondingTo)
  def whenProposalAccepted(neg: NegotiationId, pid: ProposalId)(implicit ag: Ag): Unit = if(allAccepted(neg, pid)){
    //    ag.log.info("all accepted, setting state to **Waiting** " + proposalAcceptance(neg))
    ag.get(neg).currentState update NegotiationState.Waiting
  }

  final def nothingToProposeIfHasMaxPriority(negId: NegotiationId)(implicit ag: Ag): Option[Unit] =
    if(maxPriority(negId).getOrThrow("maxPriority unknown")._1 == ag.ref) Some{
      hasNothingToProposeWhileTopPriority(negId)
    } else None

  def hasNothingToProposeWhileTopPriority(negId: NegotiationId)(implicit ag: Ag): Unit = sys.error("Failed to resolve negotiation")

  protected var pausedByFallbackRequest: Option[UUID] = None

  moreProcess <:= {
    implicit ag => {
      case msg: FallbackRequest if ag.hasState(msg, NegotiationState.Negotiating, NegotiationState.Waiting) =>
        ag.resendDelayedMessages()
        onFallbackRequest(msg)
      /* Fallback State */
      case msg: IWillMove if ag.hasState(msg, FallbackState) && pausedByFallbackRequest.contains(msg.respondingTo) =>
        val neg = ag.get(msg.negotiation)
        neg.currentIterator update ag.newIterator(neg.id)
        ag.setNextProposal(neg.id)
        neg.currentState update NegotiationState.Negotiating
        pausedByFallbackRequest = None
        ag.resendDelayedMessages()
        ag.sendToAll(neg.currentProposal())
      case msg: IWillMove if ag.hasState(msg, FallbackState, NegotiationState.Negotiating) => ag.guardDelayedMessage(msg) // for another fallback request
      case msg: FallbackRequest if ag.hasState(msg, FallbackState) => ag.guardDelayedMessage(msg)
      case msg: Message.Proposal if ag.hasState(msg, FallbackState) => ag.guardDelayedMessage(msg)
      case msg: IWillMove if ag.myPriority isLowerThenOf msg => ???
      case msg if ag.hasState(msg, FallbackState) => // do nothing
    }
  }

  def sendFallbackRequest(negId: NegotiationId)(implicit ag: Ag) = {
    val neg = ag.get(negId)
    neg.currentState update FallbackState
    val req = FallbackRequest(negId, neg.currentPriority(), UUID.randomUUID())(ag.ref)
    pausedByFallbackRequest = Some(req.id)
    ag.sendToAll(req)
  }

  nothingToPropose <:= {
    implicit ag => {
      negId =>
        clearProposalAcceptance(negId)(ag)
        nothingToProposeIfHasMaxPriority(negId) getOrElse sendFallbackRequest(negId)
    }
  }


}
