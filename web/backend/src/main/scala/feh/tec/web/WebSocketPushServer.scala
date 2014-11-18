package feh.tec.web

import akka.actor.{ActorSystem, Props, ActorRef}
import akka.io.Tcp.ConnectionClosed
import feh.tec.web.common.WebSocketMessages
import spray.can.Http
import spray.can.websocket.UpgradedToWebSocket
import spray.can.websocket.frame.{Frame, TextFrame}
import spray.json.JsonFormat

object WebSocketPushServer{
  protected[web] case class WorkerConnectionClosed(msg: ConnectionClosed)
  case class Push(msg: WebSocketMessages#Msg, format: JsonFormat[WebSocketMessages#Msg])
  case class OnConnection(frames: List[Either[Frame, () => Any]])
}

class WebSocketPushServer extends WebSocketServer{
  import WebSocketPushServer._

  var sendOnConnection: Option[List[Either[Frame, () => Any]]] = None

  def workerFor(connection: ActorRef) = Props(new WebSocketPushWorker(connection, self, sendOnConnection))

  override def receive: PartialFunction[Any, Unit] = super.receive orElse{
    case OnConnection(frames) => sendOnConnection = Option(frames)
    case p: Push =>
      workers foreach(_ forward p)
    case WorkerConnectionClosed(reason) =>
      workers -= sender()
      context stop sender()
  }


}

class WebSocketPushWorker(serverConnection: ActorRef,
                          supervisor: ActorRef,
                          sendOnConnection: Option[List[Either[Frame, () => Any]]])
  extends WebSocketWorker(serverConnection)
{
  import WebSocketPushServer._

  def businessLogic: Receive = {
    case Push(msg, format) => send(TextFrame(format.write(msg).toString()))
    case UpgradedToWebSocket => sendOnConnection.foreach(_ foreach {
      case Left(frame) => send(frame)
      case Right(func) => func()
    })
    case closed: ConnectionClosed => supervisor ! WorkerConnectionClosed(closed)
    case msg                      => log.info("message received: " + msg)
  }

}

class WebSocketPushServerInitialization(host: String, port: Int, val uniqueName: String)
                                       (implicit val asys: ActorSystem) extends WebSocketServerInitialization
{
  def bind = Http.Bind(server, host, port)
  def serverProps = Props(new WebSocketPushServer())

  bindTheServer()
}