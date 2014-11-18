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
  case class OnConnection(frames: List[ActorRef => Frame])
  object OnConnection{
    def apply(frames: (ActorRef => Frame)*) = new OnConnection(frames.toList)
  }
}

class WebSocketPushServer extends WebSocketServer{
  import WebSocketPushServer._

  var sendOnConnection: List[ActorRef => Frame] = Nil

  def workerFor(connection: ActorRef) = Props(new WebSocketPushWorker(connection, self, sendOnConnection.map(f => () => f(self))))

  override def receive: PartialFunction[Any, Unit] = super.receive orElse{
    case OnConnection(frames) => sendOnConnection :::= frames
    case p: Push =>
      workers foreach(_ forward p)
    case WorkerConnectionClosed(reason) =>
      workers -= sender()
      context stop sender()
  }


}

class WebSocketPushWorker(serverConnection: ActorRef,
                          supervisor: ActorRef,
                          sendOnConnection: List[() => Frame])
  extends WebSocketWorker(serverConnection)
{
  import WebSocketPushServer._

  def businessLogic: Receive = {
    case Push(msg, format) => send(TextFrame(format.write(msg).toString()))
    case UpgradedToWebSocket => sendOnConnection.foreach(f => send(f()))
    case closed: ConnectionClosed => supervisor ! WorkerConnectionClosed(closed)
    case msg                      => log.info("message received: " + msg)
  }

}

class WebSocketPushServerCreation(host: String, port: Int, val uniqueName: String, onConnection: WebSocketPushServer.OnConnection)
                                 (implicit val asys: ActorSystem) extends WebSocketServerCreation
{

  def bind = Http.Bind(server, host, port)
  def serverProps = Props(new WebSocketPushServer())

  bindTheServer()

  protected def initPushServer(srv: ActorRef): Unit = srv ! onConnection
}