package feh.tec.agents.lite

import feh.tec.agents.NQueenWebSocketPushServerBuilder
import feh.tec.web.WebSocketPushServer.OnConnection
import feh.tec.web.common.{NQueenMessages, WebsocketConf}
import feh.util._
import akka.actor.{ActorRef, ActorSystem}
import feh.tec.agents.lite.spec.dsl._
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
        timeout.`response delay` <= 0.millis
      )

      when finished {
        controller =>
          (neg, values) => {
            controller.log.info(s"negotiation $neg successfully finished: $values;\nShutting Down in $shutDownIn")
            controller.agents.foreach{
              ag =>
                ag.ref.tell(
                  AgentReport.StateRequest(neg),
                  WebPushServer
                )
            }
            asys.scheduler.scheduleOnce(shutDownIn, () => {
              WebPushServer.tell(SystemMessage.NegotiationFinished(neg, values), controller.self)
              asys.shutdown()
              sys.exit(0)
            })(asys.dispatcher)
          }
      }

      when failed {
        controller =>
          (neg, reason) => {
            controller.log.info(s"negotiation $neg failed: $reason")
            asys.shutdown()
            sys.exit(1)
          }

      }
    }
  }

  def N = 6

  val shutDownIn = 1 second span

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