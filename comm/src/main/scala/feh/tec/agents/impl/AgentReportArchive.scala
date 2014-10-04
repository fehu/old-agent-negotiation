package feh.tec.agents.impl

import akka.actor.{ActorLogging, ActorRef}
import feh.tec.agents.Message.AutoId
import feh.tec.agents.impl.AgentReports.ZeroTime
import feh.tec.agents.impl.ProposalEngine.SharingKnowledge
import feh.tec.agents.impl.System.Service
import feh.tec.agents.{AgentRef, SystemMessage, SystemRole}

import scala.collection.mutable
import scala.concurrent.duration.{Duration, FiniteDuration}

object ReportArchive extends SystemRole{
  val name = "ReportArchive"

  case class Forward(to: ActorRef) extends SystemMessage with AutoId
  case class BulkAndForward(to: ActorRef) extends SystemMessage with AutoId
  case class BulkReports protected[ReportArchive] (reports: List[AgentReport]) extends SystemMessage with AutoId
  case class Print(v: Boolean) extends SystemMessage with AutoId
}

abstract class ReportArchive extends ReportsRegister with ReportsPrinter{
  def role = ReportArchive

  def zeroTime: ZeroTime

  def reports: Map[AgentRef, List[AgentReport]]

  def start()

  override def processSys = super.processSys orElse{
    case SystemMessage.Start() => start()
    case _rep: AgentReport =>
      val rep = if(_rep.at.undefined) _rep.updTime(zeroTime.diff) else _rep
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
          case (negId, AgentReports.StateReportEntry(p, v, s, a, top, extra)) =>
            sb ++= " "*12 ++= s"  priority: $p\n" ++= (if(top) " (top)" else "")
            sb ++= " "*12 ++= s"    values: $v\n"
            sb ++= " "*12 ++= s"     scope: $s\n"
            sb ++= " "*12 ++= s"acceptance: $s"
            if(extra.isDefined) sb ++= ("\n" + " "*12 + s"   extra: ${extra.get}")
        }
        log.info(sb.mkString)
      case AgentReports.MessageReport(to, msg, _, extra) =>
        log.info(s"Message $msg was sent by ${msg.sender} to $to" + extra.map("; extra: " +).getOrElse(""))
    }

  }
}



class ArchiveRegisterImpl extends ReportArchive {
  protected val _reports = mutable.LinkedHashMap.empty[AgentRef, mutable.Buffer[AgentReport]]
  def reports: Map[AgentRef, List[AgentReport]] = _reports.mapValues(_.toList).toMap

  def newReport(rep: AgentReport): Unit = _reports.getOrElse(rep.of, createNewEntry(rep)) += rep

  private def createNewEntry(rep: AgentReport) = {
    val buff = mutable.Buffer.empty[AgentReport]
    _reports += rep.of -> buff
    buff
  }

  var zeroTime = ZeroTime()

  def start() = {
    _reports.clear()
    zeroTime = ZeroTime()
  }
}

object ArchiveRegisterImpl{
  type Service = ArchiveRegisterImpl with Service.Args0
}

class ReportRegisterImpl(val controlAcceptanceCheckDelay: FiniteDuration, val controller: AgentRef)
  extends ArchiveRegisterImpl with ReportsRegister.NegotiationSuccessWatcher
{
  override def start() = {
    super.start()
    acceptanceRegister.clear()
  }

  override def processSys = super.processSys orElse {
    case fail: SharingKnowledge.ConfigurationProvenFailure => notifyController(fail)
  }
}

object ReportRegisterImpl{
  type Service = ReportRegisterImpl with Service.Args2[FiniteDuration, ActorRef]

//  def apply(controlAcceptanceCheckDelay: FiniteDuration, controller: ActorRef) =
//    new ReportRegisterImpl(controlAcceptanceCheckDelay, controller) with Service.Args2[FiniteDuration, ActorRef]
}