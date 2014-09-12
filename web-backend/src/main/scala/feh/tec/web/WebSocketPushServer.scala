package feh.tec.web

import akka.actor.{ActorSystem, Props, ActorRef}
import akka.io.Tcp.ConnectionClosed
import spray.can.Http
import spray.can.Http.Bind
import spray.can.websocket.frame.TextFrame

object WebSocketPushServer{
  protected[web] case class WorkerConnectionClosed(msg: ConnectionClosed)
  case class Push(msg: String)
}

class WebSocketPushServer extends WebSocketServer{
  import WebSocketPushServer._
  
  def workerFor(connection: ActorRef) = Props(new WebSocketPushWorker(connection, self))

  override def receive: PartialFunction[Any, Unit] = super.receive orElse{
    case p: Push =>
      log.info(s"Pushing to workers $workers: ${p.msg}")
      workers foreach(_ forward p)
    case WorkerConnectionClosed(reason) =>
      workers -= sender()
      context stop sender()
  }
}

class WebSocketPushWorker(serverConnection: ActorRef, supervisor: ActorRef) extends WebSocketWorker(serverConnection){
  import WebSocketPushServer._

  def businessLogic: Receive = {
    case Push(msg)                => send(TextFrame(msg))
    case closed: ConnectionClosed => supervisor ! WorkerConnectionClosed(closed)
    case msg                      => log.info("message received: " + msg)
  }

}

class WebSocketPushServerInitialization(host: String, port: Int)
                                       (implicit val asys: ActorSystem) extends WebSocketServerInitialization
{
  def bind = Http.Bind(server, host, port)
  def serverProps = Props(new WebSocketPushServer)

  bindTheServer()
}