package feh.tec.agents.impl

import java.util.UUID

import akka.actor.ActorLogging
import feh.tec.agents.Message.AutoId
import feh.tec.agents._
import feh.tec.agents.impl.System.Service
import feh.util._

case object ReportsPrinter extends SystemRole{ val name = "ReportsPrinter" }
class ReportsPrinter extends SystemAgent with Service.Args0 with ActorLogging{
  def role = ReportsPrinter

  override def processSys: PartialFunction[SystemMessage, Unit] = super.processSys orElse{
    case AgentReports.StateReport(of, report, _) =>
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
    case AgentReports.MessageReport(to, msg) =>
      log.info(s"Message $msg was sent by ${msg.sender} to $to")
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