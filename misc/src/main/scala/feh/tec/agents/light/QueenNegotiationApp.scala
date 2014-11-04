package feh.tec.agents.light

import feh.util._
import akka.actor.ActorSystem
import feh.tec.agents.light.spec.dsl._
import impl.agent._
import scala.concurrent.duration._

object QueenNegotiationApp extends App{
//  val agentProps =
//    AgentProps("test", NegotiationRole("test"), Set(), create.PriorityAndProposalBasedIteratingAllVars(agentSpec))

  def negController(boardSize: Int) = controller {
    new Negotiation {

      var x = variable `with` domain(1 to boardSize)
      var y = variable `with` domain(1 to boardSize)

      def `queen's position` = negotiation over(x, y)

      def Queen = agent withRole "chess queen" definedBy QueenSpec that (
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