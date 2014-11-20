package feh.tec.agents

import feh.tec.agents.lite.NegotiationState._
import feh.tec.agents.lite.spec.NegotiationSpecification._
import feh.tec.agents.lite.spec.dsl._

import scala.concurrent.duration._

object AgentSpecApp extends App{
  var aNegotiation: NegotiationDef = null

  `def`(new Agent with PriorityAndProposalBased/*.HavingViews*/ with SolutionFiltering {
    start := {
      log.info("i've started")
    }

    // definition for all negotiations
    after(start) {
      spamProposal
//      await(view.hasMinimumData, 1 second)
      state = Negotiating
    }


    for (neg <- aNegotiation) {

      onProposal := {
        case proposal => respond(if (proposal breaks constraints) Rejected else Accepted)
      }

      onAcceptance := {
        case msg => /* do nothing */
      }
//
//      onRejection := {
//        case my.priority > sender.priority => ignore
//        case my.priority < sender.priority => guard(rejection)
//      }
//
//      when(neg receivesAll responses) {
//        if ((neg.currentProposal satisfies view.externalConstraints) && (SolutionFilter accepts messages))
//          neg.currentProposal is Accepted
//        else neg.currentProposal is Rejected
//      }
    }



  })
}
