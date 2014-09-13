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
}

class WebSocketPushServer(sendOnConnection: Option[Frame]) extends WebSocketServer{
  import WebSocketPushServer._
  
  def workerFor(connection: ActorRef) = Props(new WebSocketPushWorker(connection, self, sendOnConnection))

  override def receive: PartialFunction[Any, Unit] = super.receive orElse{
    case p: Push =>
      log.info(s"Pushing to workers $workers: ${p.msg}")
      workers foreach(_ forward p)
    case WorkerConnectionClosed(reason) =>
      workers -= sender()
      context stop sender()
  }


}

class WebSocketPushWorker(serverConnection: ActorRef, supervisor: ActorRef, sendOnConnection: Option[Frame])
  extends WebSocketWorker(serverConnection)
{
  import WebSocketPushServer._

  def businessLogic: Receive = {
    case Push(msg, format) => send(TextFrame(format.write(msg).toString()))
    case UpgradedToWebSocket => sendOnConnection.foreach(send)
    case closed: ConnectionClosed => supervisor ! WorkerConnectionClosed(closed)
    case msg                      => log.info("message received: " + msg)
  }

}

class WebSocketPushServerInitialization(host: String, port: Int)
                                       (implicit val asys: ActorSystem) extends WebSocketServerInitialization
{
  def bind = Http.Bind(server, host, port)
  def sendOnConnection: Option[Frame] = None
  def serverProps() = Props(new WebSocketPushServer(sendOnConnection))

  bindTheServer()
}