package feh.tec.agents.light

import feh.tec.agents.NQueenWebSocketPushServer
import feh.util._
import akka.actor.{Props, ActorSystem}
import feh.tec.agents.light.spec.dsl._
import impl.agent._
import scala.concurrent.duration._

object QueenNegotiationApp extends App{

  def negController(boardSize: Int) = controller {
    new Negotiation {

      var x = variable `with` domain(1 to boardSize)
      var y = variable `with` domain(1 to boardSize)

      def `queen's position` = negotiation over(x, y)

      def Queen = agent withRole "chess queen" definedBy QueenSpec() that (
        negotiates the `queen's position` `with` the.others reportingTo reporter.forwarding(WebPushServer) and
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
        timeout.initialize <= 100.millis,
        timeout.start <= 100.millis
      )
    }
  }

  val props = negController(4)

  implicit val asys = ActorSystem()

  lazy val WebPushServer = asys.actorOf(Props(new NQueenWebSocketPushServer(NegotiationId("queen's position"), 200 millis)))

  val c = asys.actorOf(props)

  c ! SystemMessage.Initialize
  Thread sleep 100
  c ! SystemMessage.Start
}