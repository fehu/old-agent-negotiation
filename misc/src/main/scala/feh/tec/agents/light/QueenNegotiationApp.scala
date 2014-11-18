package feh.tec.agents.light

import feh.tec.agents.{NQueenWebSocketPushServerBuilder, NQueenWebSocketPushServer}
import feh.tec.web.WebSocketPushServer
import feh.tec.web.WebSocketPushServer.OnConnection
import feh.tec.web.common.{NQueenMessages, WebsocketConf}
import feh.util._
import akka.actor.{ActorRef, Props, ActorSystem}
import feh.tec.agents.light.spec.dsl._
import impl.agent._
import spray.can.websocket.frame.TextFrame
import scala.concurrent.duration._

object QueenNegotiationApp extends App with WebsocketConf{

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
        timeout.start <= 100.millis,
        timeout.`response delay` <= 200.millis
      )
    }
  }

  def N = 4

  implicit val asys = ActorSystem()

  def pushServerBuilder = {
    val initMessage = NQueenMessages.Init(for(i <- 1 to N) yield NQueenMessages.Queen(i) -> "Queen")

    import spray.json._
    import feh.tec.web.NQueenProtocol._

    new NQueenWebSocketPushServerBuilder(

      wsConf.back.host("n-queen"),
      wsConf.back.port("n-queen"),
      negotiationId = NegotiationId("queen's position"),
      flushFrequency = 200 millis,
      OnConnection(implicit self => {
        c ! AgentReport.StateRequest(NegotiationId("queen's position"))
        TextFrame(initMessage.toJson.toString())
      })
    )
  }

  lazy val WebPushServer: ActorRef = pushServerBuilder.server

  val props = negController(N)

  WebPushServer

  val c = asys.actorOf(props)

  c ! SystemMessage.Initialize
  Thread sleep 100
  c ! SystemMessage.Start
}