package feh.tec.agents.light

import feh.tec.agents.light.NegotiationState.{Waiting, Negotiating}
import feh.tec.agents.light.spec.NegotiationSpecification.NegotiationDef
import feh.tec.agents.light.spec.dsl._

trait AgentSpecTest extends Agent with PriorityAndProposalBased{
  def aNegotiation: NegotiationDef

  start := {
    log.info("i've started")
  }

  // definition for all negotiations
  after(start){
    spamProposal
    await(view.hasMinimumData)
    state = Negotiating
  }

  for(neg <- aNegotiation){

    onProposal := {
      case proposal => respond(if(proposal breaks constraints) Rejected else Accepted)
    }

    onAcceptance := {
      case msg => /* do nothing */
    }

    onRejection := {
      case agent.priority > sender.priority => ignore
      case agent.priority < sender.priority => guard(rejection)
    }

    when(neg receivesAll responses){
      if((neg.currentProposal satisfies view.externalConstraints) && (solutionFilter accepts messages))
        neg.currentProposal is Accepted
      else neg.currentProposal is Rejected
    }

    onProposalAcceptance := {
      case _ => state = Waiting
    }

    onProposalRejection{

    }

  }
