package feh.tec.agents.light

import akka.actor.ActorSystem
import feh.tec.agents.light.spec.dsl._
import impl.agent._
import scala.concurrent.duration._

object AgentManualSpecApp extends App{
  val agentSpec = new create.PPI.AllVarsSpec {

    initialize after { ag => _ => ag.log.info("initialized") }
    start andThen {
      ag =>
        import ag._
        negotiations.foreach {
          neg =>
            val proposal = neg.currentProposal.getOrElse {
              Message.Proposal(Message.ProposalId.rand, neg.id, neg.currentPriority(), neg.currentValues())
            }
            neg.currentState update NegotiationState.Negotiating
            log.info(s"negotiation ${neg.id} started, scope = ${neg.scope}")
            sendToAll(proposal)
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
        negotiates the `queen's position` `with` the.others and
          hasConstraints(
            "direct-line sight" | {
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

  val ag = asys.actorOf(props)

  ag ! SystemMessage.Initialize
}