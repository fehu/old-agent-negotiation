package feh.tec.web

import akka.actor._
import feh.tec.web.WebSocketPushServer.Push

import scala.concurrent.duration._

object WebSocketTestSrv extends WebSocketPushServerInitialization("localhost", 8080)(ActorSystem.create("WebSocketTestSrv"))
  with App
{
  import asys.dispatcher

  asys.scheduler.schedule(1 second, 1 second, server, Push("Testing"))
}
