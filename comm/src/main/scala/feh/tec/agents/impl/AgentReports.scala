package feh.tec.agents.impl

import java.util.{Date, UUID}

import akka.actor.{ActorLogging, ActorRef}
import feh.tec.agents.Message.AutoId
import feh.tec.agents.SystemMessage.NegotiationFinished
import feh.tec.agents._
import feh.tec.agents.impl.AgentReports.{StateReport, TimeDiff}
import feh.util._

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

trait AgentReport extends SystemMessage{
  def of: AgentRef
  def at: TimeDiff

  def updTime(time: TimeDiff): AgentReport
}

object AgentReports{
  case class ZeroTime(date: Date = new Date){
    def diff = TimeDiff( (java.lang.System.currentTimeMillis() - date.getTime).toInt )
  }

  case class TimeDiff(diff: Int){
    def defined = true
    final def undefined = !defined
  }

  object TimeDiffUndefined extends TimeDiff(-1){
    override def defined = false
    def unapply(diff: TimeDiff): Boolean = diff.diff == -1
  }

  case class ReportStates(of: NegotiationId*) extends SystemMessage with AutoId{
    def response(f: NegotiationId => Option[StateReportEntry])(implicit responding: AgentRef) =
      of.zipMap(f).map {
        case (negId, Some(t)) => negId -> t
        case (negId, None)    => negId -> null
      }.toMap |> (StateReport(responding, _, TimeDiffUndefined, id))
  }

  case class ReportAllStates() extends SystemMessage with AutoId{
    def response(res: Map[NegotiationId, AgentReports.type => StateReportEntry])(implicit responding: AgentRef) =
      res.map {
        case (negId, build) => negId -> build(AgentReports)
      }.toMap |> (StateReport(responding, _, TimeDiffUndefined, id))
  }

  case class StateReport(of: AgentRef,
                         report: Map[NegotiationId, StateReportEntry],
                         at: TimeDiff,
                         id: Message.Id = UUID.randomUUID()
                          ) extends AgentReport
  {
    def updTime(time: TimeDiff) = copy(at = time)
  }

  case class StateReportEntry(priority: Priority,
                              vals: Map[Var, Any],
                              scope: Set[AgentRef],
                              currentValuesAcceptance: Boolean,
                              extra: Option[Any])

  case class MessageReport(to: AgentRef, msg: Language#Msg, at: TimeDiff, extra: Option[MessageReportExtra])
    extends AgentReport with AutoId
  {
    def of = msg.sender
    def updTime(time: TimeDiff) = copy(at = time)
  }

  trait MessageReportExtra

  case class WeightReport(weighted: Map[Option[Boolean], InUnitInterval]) extends MessageReportExtra
}

trait ReportsRegister extends SystemAgent{
  def newReport(rep: AgentReport)
}

object ReportsRegister{
  case class ControlAcceptanceCheck(of: NegotiationId) extends SystemMessage with AutoId

  trait ControlNotifier{
    def controller: AgentRef

    def notifyController(msg: SystemMessage) = controller.ref ! msg
  }

  trait NegotiationSuccessWatcher extends ReportsRegister with ControlNotifier with ActorLogging{

    def controlAcceptanceCheckDelay: FiniteDuration

    protected val acceptanceRegister = mutable.HashMap.empty[(AgentRef, NegotiationId), Boolean]

    abstract override def newReport(rep: AgentReport) = {
      super.newReport(rep)
      registerAcceptance(rep)
      checkAcceptance().withFilter(_._2) foreach (scheduleControlAcceptanceCheck _).compose(_._1)
    }

    override def processSys = super.processSys orElse {
      case ControlAcceptanceCheck(neg) => if(checkAcceptance(neg)) notifyController(NegotiationFinished(neg))
    }

    protected def registerAcceptance(rep: AgentReport) = rep match {
      case StateReport(ag, reps, _, _) => reps.foreach{
        case (neg, entry) => acceptanceRegister += (ag, neg) -> entry.currentValuesAcceptance        
      }
      case _ =>
    }

    protected def checkAcceptance(): Map[NegotiationId, Boolean] = acceptanceRegister.groupBy(_._1._2) mapValues {
        _.values.reduceLeftOption(_ && _) getOrElse false
      }
    protected def checkAcceptance(neg: NegotiationId): Boolean = acceptanceRegister.withFilter(_._1._2 == neg)
      .map(_._2).reduceLeftOption(_ && _) getOrElse false

    protected def scheduleControlAcceptanceCheck(of: NegotiationId) = context.system.scheduler
      .scheduleOnce(controlAcceptanceCheckDelay, self, ControlAcceptanceCheck(of))(context.dispatcher)
  }
}