package feh.tec.agents.impl

import java.util.UUID

import akka.actor.ActorLogging
import feh.tec.agents.Message.AutoId
import feh.tec.agents._
import feh.tec.agents.impl.System.Service
import feh.util._

import scala.collection.mutable

case object ReportsPrinter extends SystemRole{
  val name = "ReportsPrinter"

  case class Forward(to: AgentRef) extends SystemMessage with AutoId
  case class Silent(v: Boolean) extends SystemMessage with AutoId
}
class ReportsPrinter extends SystemAgent with Service.Args0 with ActorLogging{
  def role = ReportsPrinter

  var silent = false
  val forwarding = mutable.HashSet.empty[AgentRef]

  override def processSys: PartialFunction[SystemMessage, Unit] = super.processSys orElse{
    case SystemMessage.Start() => // do nothing
    case rep@AgentReports.StateReport(of, report, _) if !silent =>
      forwarding.foreach(_.ref ! rep)
      val sb = new StringBuilder
      sb ++= s"Report by $of:\n"
      report foreach {
        case (negId, AgentReports.StateReportEntry(p, v, s, extra)) =>
          sb ++= (" "*12 + s"priority: $p\n")
          sb ++= (" "*12 + s"  values: $v\n")
          sb ++= (" "*12 + s"   scope: $s")
          if(extra.isDefined) sb ++= ("\n" + " "*12 + s"   extra: ${extra.get}")
      }
      log.info(sb.mkString)
    case rep: AgentReports.StateReport =>
      forwarding.foreach(_.ref ! rep)
    case rep@AgentReports.MessageReport(to, msg) if !silent=>
      forwarding.foreach(_.ref ! rep)
      log.info(s"Message $msg was sent by ${msg.sender} to $to")
    case rep: AgentReports.MessageReport =>
      forwarding.foreach(_.ref ! rep)
    case ReportsPrinter.Forward(to) =>
      log.info(s"Forwarding to $to registered")
      forwarding += to
    case ReportsPrinter.Silent(v) => silent = v
  }
}

object ReportArchive extends SystemRole{ val name = "ReportArchive" }
abstract class ReportArchive extends SystemAgent with Service.Args0 with ActorLogging {
  def role = ReportArchive

  def reports: Map[AgentRef, Seq[AgentReport]]

  def newReport(rep: AgentReport)

  override def processSys = super.processSys orElse{
    case rep: AgentReport => newReport(rep)
  }
}

trait AgentReport extends SystemMessage{
  def of: AgentRef
}

object AgentReports{

  case class ReportStates(of: NegotiationId*) extends SystemMessage with AutoId{
    def response(f: NegotiationId => Option[(Priority, Map[Var, Any], Set[AgentRef], Option[Any])])(implicit responding: AgentRef) =
      of.zipMap(f).map {
        case (negId, Some(t)) => negId -> StateReportEntry.tupled(t)
        case (negId, None)    => negId -> null
      }.toMap |> (StateReport(responding, _, id))
  }

  case class ReportAllStates() extends SystemMessage with AutoId{
    def response(res: Map[NegotiationId, (Priority, Map[Var, Any], Set[AgentRef], Option[Any])])(implicit responding: AgentRef) =
      res.map {
        case (negId, (p, v, s, e)) => negId -> StateReportEntry(p, v, s, e)
      }.toMap |> (StateReport(responding, _, id))
  }

  case class StateReport(of: AgentRef, report: Map[NegotiationId, StateReportEntry], id: Message.Id = UUID.randomUUID()) extends AgentReport
  case class StateReportEntry(priority: Priority,
                              vals: Map[Var, Any],
                              scope: Set[AgentRef],
                              extra: Option[Any])

  case class MessageReport(to: AgentRef, msg: Language#Msg) extends AgentReport with AutoId { def of = msg.sender }

}