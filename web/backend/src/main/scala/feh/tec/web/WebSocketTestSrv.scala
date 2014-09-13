package feh.tec.web

import akka.actor._
import feh.tec.web.WebSocketPushServer.Push
import feh.tec.web.common.{WebSocketMessages, NQueenMessages}
import feh.tec.web.common.NQueenMessages._
import spray.can.websocket.frame.{TextFrame, Frame}
import spray.json._

import scala.concurrent.duration._
import scala.reflect.ClassTag

object WebSocketTestSrv extends WebSocketPushServerInitialization("localhost", 8080)(ActorSystem.create("WebSocketTestSrv"))
  with App
{
  import asys.dispatcher
  import NQueenProtocol._

  lazy val queens = (1 to 4).map{
    i => Queen(i) -> "Queen"
  }

  override def sendOnConnection: Option[Frame] = Some(TextFrame( Init(queens).toJson.toString() ))


  asys.scheduler.schedule(0 millis,   1 second, server, push(StateReport(Queen(1), (1, 1), 1, Seq(Queen(2) -> true))))
  asys.scheduler.schedule(250 millis, 1 second, server, push(StateReport(Queen(2), (1, 1), 2, Nil)))
  asys.scheduler.schedule(500 millis, 1 second, server, push(StateReport(Queen(3), (1, 1), 3, Nil)))
  asys.scheduler.schedule(750 millis, 1 second, server, push(StateReport(Queen(4), (1, 1), 4, Nil)))


  def push[Msg <: NQueenMessages.Msg : JsonFormat](msg: Msg) =
    Push(msg, implicitly[JsonFormat[Msg]].asInstanceOf[JsonFormat[WebSocketMessages#Msg]])
}

object NQueenProtocol extends DefaultJsonProtocol{
  abstract class NamedFormat[T: ClassTag](oldFormat: JsonFormat[T]) extends JsonFormat[T]{
    def write(obj: T): JsValue = {
      val old = oldFormat.write(obj).asJsObject
      old.copy(old.fields + ("$t" -> JsString(implicitly[ClassTag[T]].runtimeClass.getSimpleName)))
    }
    def read(json: JsValue): T = ???
  }
  
  implicit object QueenFormat extends NamedFormat(jsonFormat1(Queen))
  implicit lazy val InitFormat: JsonFormat[Init] = new NamedFormat(jsonFormat1(Init)){}
  implicit lazy val StateReportFormat: JsonFormat[StateReport] = new NamedFormat(jsonFormat4(StateReport)){}

  implicit object MessageTypeFormat extends RootJsonFormat[MessageType]{
    def write(obj: MessageType): JsValue = obj match {
      case Proposal   => JsString("Proposal")
      case Acceptance => JsString("Acceptance")
      case Rejection  => JsString("Rejection")
    }
    def read(json: JsValue): MessageType = ???
  }

  implicit object messageFormat extends NamedFormat(jsonFormat3(Message))
  implicit object messageSentFormat extends NamedFormat(jsonFormat3(MessageReport))

  /*  implicit object ProposalFormat extends RootJsonFormat[Proposal.type]{
    def write(obj: Proposal.type): JsValue = JsString("Proposal")
    def read(json: JsValue): Proposal.type = ???
  }

  implicit object AcceptanceFormat extends RootJsonFormat[Acceptance.type]{
    def write(obj: Acceptance.type): JsValue = JsString("Acceptance")
    def read(json: JsValue): Acceptance.type = ???
  }

  implicit object RejectionFormat extends RootJsonFormat[Rejection.type]{
    def write(obj: Rejection.type): JsValue = JsString("Rejection")
    def read(json: JsValue): Rejection.type = ???
  }*/

}