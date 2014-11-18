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

  abstract class WriteOnlyFormat[T](writef: T => JsValue)extends RootJsonFormat[T]{
    def write(obj: T) = writef(obj)
    def read(json: JsValue) = ???
  }
  
  implicit object QueenFormat extends NamedFormat(jsonFormat1(Queen))
  implicit lazy val InitFormat: JsonFormat[Init] = new NamedFormat(jsonFormat1(Init)){}
  implicit lazy val StateReportFormat: JsonFormat[StateReport] = new NamedFormat(jsonFormat7(StateReport)){}
  implicit lazy val ChangeReportFormat: JsonFormat[ChangeReport] = new NamedFormat(jsonFormat4(ChangeReport)){}

  implicit object CanBulkFormat extends WriteOnlyFormat[CanBulk] ({
    case state: StateReport => StateReportFormat.write(state)
    case state: ChangeReport => ChangeReportFormat.write(state)
    case msg: MessageReport => MessageSentFormat.write(msg)
  })

  implicit lazy val BulkReportFormat: JsonFormat[BulkReport] = new NamedFormat(jsonFormat1(BulkReport)) {}

  implicit object MessageTypeFormat extends WriteOnlyFormat[MessageType]({
      case Acceptance => JsString("Acceptance")
      case Rejection  => JsString("Rejection")
  })

  implicit object MessageExtraReportFormat extends RootJsonFormat[MessageExtraReport]{
    lazy val ReportWeightFormat = new NamedFormat[ReportWeight](jsonFormat1(ReportWeight)) {}

    def write(obj: MessageExtraReport) = obj match{
      case rep@ReportWeight(weight) => ReportWeightFormat.write(rep)
    }
    def read(json: JsValue) = ???
  }

  implicit object MessageContent extends RootJsonFormat[MessageContent]{
    lazy val ProposalFormat = new NamedFormat[Proposal](jsonFormat2(Proposal)) {}
    lazy val ResponseFormat = new NamedFormat[Response](jsonFormat2(Response(_: String, _: MessageType))) {}

    def write(obj: MessageContent) = obj match {
      case prop: Proposal => ProposalFormat.write(prop)
      case resp: Response => ResponseFormat.write(resp)
    }
    def read(json: JsValue) = ???
  }

  implicit lazy val MessageFormat: JsonFormat[Message] = new NamedFormat(jsonFormat3(Message)){}
  implicit lazy val MessageSentFormat: JsonFormat[MessageReport] = new NamedFormat(jsonFormat4(MessageReport)){}

  implicit object NegotiationFinishedFormat extends WriteOnlyFormat[NegotiationFinished.type](
    _ => JsObject("$t" -> JsString("NegotiationFinished"))
  )

  implicit object RestartFormat extends WriteOnlyFormat[Restart.type](
    _ => JsObject("$t" -> JsString("Restart"))
  )

  implicit lazy val NegotiationFinishedAutoRestartFormat =
    new NamedFormat[NegotiationFinishedAutoRestart](jsonFormat1(NegotiationFinishedAutoRestart)) {}

  implicit lazy val PositionProvenFailureProtocol =
    new NamedFormat[PositionProvenFailure](jsonFormat1(PositionProvenFailure)) {}
}