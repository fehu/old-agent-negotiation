package feh.tec.web

import feh.tec.web.common.NQueenMessages
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
  implicit lazy val StateReportFormat: JsonFormat[StateReport] = new NamedFormat(jsonFormat6(StateReport)){}

  implicit object CanBulkFormat extends RootJsonFormat[CanBulk] {
    def write(obj: NQueenMessages.CanBulk): JsValue = obj match {
      case state: StateReport => StateReportFormat.write(state)
      case msg: MessageReport => MessageSentFormat.write(msg)
    }
    def read(json: JsValue): NQueenMessages.CanBulk = ???
  }

  implicit lazy val BulkReportFormat: JsonFormat[BulkReport] = new NamedFormat(jsonFormat1(BulkReport)) {}

  implicit object MessageTypeFormat extends RootJsonFormat[MessageType]{
    def write(obj: MessageType): JsValue = obj match {
      case Acceptance => JsString("Acceptance")
      case Rejection  => JsString("Rejection")
    }
    def read(json: JsValue): MessageType = ???
  }

  implicit object MessageExtraReportFormat extends RootJsonFormat[MessageExtraReport]{
    lazy val ReportWeightFormat = new NamedFormat[ReportWeight](jsonFormat1(ReportWeight)) {}

    def write(obj: MessageExtraReport) = obj match{
      case rep@ReportWeight(weight) => ReportWeightFormat.write(rep)
    }
    def read(json: JsValue) = ???
  }

  implicit object MessageContent extends RootJsonFormat[MessageContent]{
    lazy val ProposalFormat = new NamedFormat[Proposal](jsonFormat1(Proposal)) {}
    lazy val ResponseFormat = new NamedFormat[Response](jsonFormat2(Response(_: String, _: MessageType))) {}

    def write(obj: MessageContent) = obj match {
      case prop: Proposal => ProposalFormat.write(prop)
      case resp: Response => ResponseFormat.write(resp)
    }
    def read(json: JsValue) = ???
  }

  implicit lazy val MessageFormat: JsonFormat[Message] = new NamedFormat(jsonFormat3(Message)){}
  implicit lazy val MessageSentFormat: JsonFormat[MessageReport] = new NamedFormat(jsonFormat5(MessageReport)){}

  implicit object NegotiationFinishedFormat extends JsonFormat[NegotiationFinished.type]{
    def write(obj: NegotiationFinished.type) = JsObject("$t" -> JsString("NegotiationFinished"))
    def read(json: JsValue) = ???
  }
}