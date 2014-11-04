package feh.tec.agents.light

import feh.util._
import akka.actor.ActorSystem
import feh.tec.agents.light.spec.dsl._
import impl.agent._
import scala.collection.mutable
import scala.concurrent.duration._

object AgentManualSpecApp extends App{
  val agentSpec = new create.PPI.AllVarsSpec {

    val priorities = mutable.HashMap.empty[AgentRef, Priority]
    val proposalAcceptance = mutable.HashMap.empty[NegotiationId, mutable.HashMap[AgentRef, Message.ProposalResponse]]

    initialize after {
      ag => _ =>
        ag.negotiations.foreach(_.currentPriority update new Priority(1))
        ag.log.info("initialized")
    }
    start andThen {
      ag =>
        import ag._
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
          priorities += msg.sender -> msg.priority
          respond(response)
      }
    }

    onProposal <:= {
      implicit ag =>{
        case msg if ag.myPriority isHigherThenOf msg => respondToProposal(msg)
        case msg if ag.myPriority isLowerThenOf  msg => respondToProposal(msg)
        case samePriority => ag.requestPriorityRaise(samePriority.negotiation)
      }
    }

    protected def onAcceptanceAndRejectionIfMyPriorityIsHigher(resp: Message.ProposalResponse) = {
      proposalAcceptance.getOrElseUpdate(resp.negotiation, mutable.HashMap.empty) += resp.sender -> resp
      priorities += resp.sender -> resp.priority
    }

    onAcceptance <:= {
      implicit ag => {
        case msg if ag.myPriority isHigherThenOf msg =>
      }
    }

    priorityNegotiationHandler{
      _.onPriorityUpdate <:= {
        implicit ag => {
          case (negId, Some(newPriority)) =>
            ag.get(negId).currentPriority update newPriority
            ag.updateCurrentProposal(negId)
            ag.sendToAll(proposal(negId))
          case (negId, None) => // do nothing, await the next proposal (by the winner)
        }
      }
    }

  }

//  val agentProps =
//    AgentProps("test", NegotiationRole("test"), Set(), create.PriorityAndProposalBasedIteratingAllVars(agentSpec))

  def negController(boardSize: Int) = controller {
    new Negotiation {

      var x = variable `with` domain(1 to boardSize)
      var y = variable `with` domain(1 to boardSize)

      def `queen's position` = negotiation over(x, y)

      def Queen = agent withRole "chess queen" definedBy agentSpec that (
        negotiates the `queen's position` `with` the.others reportingTo reporter.default and
          hasConstraints(
            "direct-line sight" | {
              /* Constraints that can be run independently for vars should be separated by && or ||, or defined separately */
              proposed(x) != valueOf(x) && proposed(y) != valueOf(y)
            },
            "diagonal-line sight" | {
              (proposed(x) - valueOf(x)).abs != (proposed(y) - valueOf(y)).abs
            }
          )
        )

      spawn agents (
        Queen -> boardSize
        )

      configure(
        timeout.creation <= 100.millis,
        timeout.`resolve conflict` <= 100.millis
      )
    }
  }

  val props = negController(4)

  implicit val asys = ActorSystem()

  val c = asys.actorOf(props)

  c ! SystemMessage.Initialize
  Thread sleep 100
  c ! SystemMessage.Start
}