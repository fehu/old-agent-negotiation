package feh.tec.agents.light

import scala.concurrent.duration._
import feh.tec.agents.light.NegotiationState.{Waiting, Negotiating}
import feh.tec.agents.light.spec.NegotiationSpecification.NegotiationDef
import feh.tec.agents.light.spec.dsl._

trait AgentSpecTest extends Agent
  with PriorityAndProposalBased.HavingViews
//  with DomainIterating.AllAsOne
  with SolutionFiltering {
  //.andSharing {
  def aNegotiation: NegotiationDef

  start := {
    log.info("i've started")
  }

  // definition for all negotiations
  after(start) {
    spamProposal
    await(view.hasMinimumData, 1 second)
    state = Negotiating
  }

  for (neg <- aNegotiation) {

    onProposal := {
      case proposal => respond(if (proposal breaks constraints) Rejected else Accepted)
    }

    onAcceptance := {
      case msg => /* do nothing */
    }

    onRejection := {
      case my.priority > sender.priority => ignore
      case my.priority < sender.priority => guard(rejection)
    }

    when(neg receivesAll responses) {
      if ((neg.currentProposal satisfies view.externalConstraints) && (SolutionFilter accepts messages))
        neg.currentProposal is Accepted
      else neg.currentProposal is Rejected
    }

//    onProposalAcceptance := {
//      state = Waiting
//    }

//    onProposalRejection {
//
//    }


//    def keepOrRaise(pNeg: PriorityNegotiation) = {
//      val requestByPriority = pNeg get requests andThen (_.orderBy(_.priority).zipWithIndex)
//      val (index, req) = requestByPriority.find(_._2.sender === agent.ref).get
//      if(index < (scope.size + 1.0) / 2) Raise else Keep
//    }
//
//    def raisePriority(pNeg: PriorityNegotiation) = {
//      val resp = pNeg get responses
//    }
//
//    def priorityNegotiation(pNeg: PriorityNegotiation) = {
//      spam(pNeg.request)
//      when(pNeg receivesAll requests, respond( keepOrRaise(pNeg) )) and {
//        case Keep =>
//        case Raise => when(pNeg receivesAll responses, )
//      }
//    }
//
//    define priority negotiation := priorityNegotiation
//    when(noMoreProposals, start priority negotiation)(priorityNegotiation)
  }

}
