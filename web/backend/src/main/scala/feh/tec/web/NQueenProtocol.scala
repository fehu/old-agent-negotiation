package feh.tec.web

import feh.tec.web.common.NQueenMessages._
import spray.json._

import scala.reflect.ClassTag

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

}