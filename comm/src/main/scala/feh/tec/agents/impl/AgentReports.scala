package feh.tec.agents.impl

import java.util.{Date, UUID}

import akka.actor.{ActorRef, ActorLogging}
import feh.tec.agents.Message.AutoId
import feh.tec.agents._
import feh.tec.agents.impl.AgentReports.TimeDiff
import feh.tec.agents.impl.System.Service
import feh.util._

import scala.collection.mutable

object ReportArchive extends SystemRole{ 
  val name = "ReportArchive"

  case class Forward(to: ActorRef) extends SystemMessage with AutoId
  case class BulkAndForward(to: ActorRef) extends SystemMessage with AutoId
  case class BulkReports protected[ReportArchive] (reports: List[AgentReport]) extends SystemMessage with AutoId
  case class Print(v: Boolean) extends SystemMessage with AutoId
}

abstract class ReportArchive extends SystemAgent with Service.Args0 with ReportsPrinter{
  def role = ReportArchive

  def reports: Map[AgentRef, List[AgentReport]]

  def newReport(rep: AgentReport)

  override def processSys = super.processSys orElse{
    case SystemMessage.Start() => // do nothing
    case rep: AgentReport =>
      newReport(rep)
      forwarding.foreach(_ ! rep)
      printAgentReport(rep)
    case ReportArchive.Forward(to) => forwarding += to
    case ReportArchive.BulkAndForward(to) =>
      forwarding += to
      to ! ReportArchive.BulkReports(reports.values.flatten.toList)
    case ReportArchive.Print(v) => printing = v
  }

}

class ReportArchiveImpl extends ReportArchive{
  protected val _reports = mutable.LinkedHashMap.empty[AgentRef, mutable.Buffer[AgentReport]]
  def reports: Map[AgentRef, List[AgentReport]] = _reports.mapValues(_.toList).toMap

  def newReport(rep: AgentReport): Unit = _reports.getOrElse(rep.of, createNewEntry(rep)) += rep

  private def createNewEntry(rep: AgentReport) = {
    val buff = mutable.Buffer.empty[AgentReport]
    _reports += rep.of -> buff
    buff
  }
}


trait ReportsPrinter extends ActorLogging{
  self: ReportArchive =>


  var printing = false
  val forwarding = mutable.HashSet.empty[ActorRef]

  def printAgentReport(rep: AgentReport) = if(printing) {
    rep match{
      case AgentReports.StateReport(of, report, _, _) =>
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
      case AgentReports.MessageReport(to, msg, _) =>
        log.info(s"Message $msg was sent by ${msg.sender} to $to")
    }

  }
}


trait AgentReport extends SystemMessage{
  def of: AgentRef
  def at: TimeDiff
}

object AgentReports{
  case class ZeroTime(date: Date = new Date){
    def diff = TimeDiff( (java.lang.System.currentTimeMillis() - date.getTime).toInt )
  }

  case class TimeDiff(diff: Int)

  object TimeDiffUndefined extends TimeDiff(-1){
    def unapply(diff: TimeDiff): Boolean = diff.diff == -1
  }

  case class ReportStates(of: NegotiationId*) extends SystemMessage with AutoId{
    def response(f: NegotiationId => Option[(Priority, Map[Var, Any], Set[AgentRef], Option[Any])])(implicit responding: AgentRef) =
      of.zipMap(f).map {
        case (negId, Some(t)) => negId -> StateReportEntry.tupled(t)
        case (negId, None)    => negId -> null
      }.toMap |> (StateReport(responding, _, TimeDiffUndefined, id))
  }

  case class ReportAllStates() extends SystemMessage with AutoId{
    def response(res: Map[NegotiationId, (Priority, Map[Var, Any], Set[AgentRef], Option[Any])])(implicit responding: AgentRef) =
      res.map {
        case (negId, (p, v, s, e)) => negId -> StateReportEntry(p, v, s, e)
      }.toMap |> (StateReport(responding, _, TimeDiffUndefined, id))
  }

  case class StateReport(of: AgentRef,
                         report: Map[NegotiationId, StateReportEntry],
                         at: TimeDiff,
                         id: Message.Id = UUID.randomUUID()
                          ) extends AgentReport

  case class StateReportEntry(priority: Priority,
                              vals: Map[Var, Any],
                              scope: Set[AgentRef],
                              extra: Option[Any])

  case class MessageReport(to: AgentRef, msg: Language#Msg, at: TimeDiff)
    extends AgentReport with AutoId { def of = msg.sender }

}