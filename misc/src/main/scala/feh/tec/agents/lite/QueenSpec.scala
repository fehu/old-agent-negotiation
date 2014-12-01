package feh.tec.agents.lite

import feh.tec.agents.lite.Fallback.FallbackState
import feh.tec.agents.lite.impl.agent.create
import feh.tec.agents.lite.impl.spec.ChangingIssuesSpec
import feh.tec.agents.lite.impl.{ChangingIssues, FailedConfigurationsChecks}
import feh.tec.agents.lite.spec.RequiresDistinctPriority

import scala.reflect.ClassTag

object QueenSpec{
  def apply() = new QueenSpec

  type Lang = create.PPI.Lang with Language.NegotiatesIssues
  type Agent = create.PPI.Ag[Lang] with FailedConfigurationsChecks[Lang] with ChangingIssues[Lang]{
    type Negotiation <: Negotiation.HasProposal[Lang] with Negotiation.HasPriority with Negotiation.HasIterator with Negotiation.ChangingIssues
  }
}

import feh.tec.agents.lite.QueenSpec._

class QueenSpec(implicit val agentTag: ClassTag[Agent]) extends create.PPI.AllVarsSpec[Agent, Lang]
  with ChangingIssuesSpec[Agent, Lang] with RequiresDistinctPriority with FallbackSpec[Agent, Lang]
{
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