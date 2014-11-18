package feh.tec.web

import akka.actor._
import akka.io.IO
import spray.can.server.UHttp
import spray.can.{websocket, Http}
import spray.routing.HttpServiceActor
import feh.util._

import scala.collection.mutable

abstract class WebSocketServer extends Actor with ActorLogging {
  def workerFor(connection: ActorRef): Props

  val workers = mutable.HashSet.empty[ActorRef]

  def receive = {
    // when a new connection comes in we register a WebSocketConnection actor as the per connection handler
    case Http.Connected(remoteAddress, localAddress) =>
      log.info(s"Http.Connected($remoteAddress, $localAddress)")
      val serverConnection = sender()
      val conn = context.actorOf(workerFor(serverConnection))
      log.info(s"New worker: " + conn)
      workers += conn
      log.info(s"workers: " + workers)
      serverConnection ! Http.Register(conn)
  }
}

abstract class WebSocketWorker(val serverConnection: ActorRef) extends HttpServiceActor
  with websocket.WebSocketServerWorker


trait WebSocketServerCreation{
  implicit val asys: ActorSystem

  def uniqueName: String
  def serverProps: Props
  def bind: Http.Bind

  lazy val server = create()
  protected def create() = asys.actorOf(serverProps, uniqueName) $$ initPushServer _

  def bindTheServer() = IO(UHttp) ! bind
  
  protected def initPushServer(srv: ActorRef)
}