package feh.tec.agents.lite

import java.io.{FileWriter, BufferedWriter, File}

import akka.actor.{Actor, ActorLogging, ActorRef}
import feh.tec.agents.lite.impl.NegotiationEnvironmentController
import org.apache.commons.io.IOUtils
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

trait AgentReport extends Message { def at: Long }

case class MessageReport(msg: NegotiationLanguage#Msg,
                         to: AgentRef,
                         extraMessage: Map[String, Any],
                         at: Long = System.currentTimeMillis()
                          ) extends AgentReport{
  val negotiation = msg.negotiation
  val sender = msg.sender
  def asString = s"""MessageReport by $sender to $to: "$msg" """
}

case class StateReport(negotiation: NegotiationId, state: Map[String, Any], description: String, at: Long = System.currentTimeMillis())
                      (implicit val sender: AgentRef) extends AgentReport{

  def asString = s"StateReport by $sender: $negotiation -> $state"
}

object AgentReport{
  case class StateRequest(neg: NegotiationId) extends SystemMessage

  case class Forward(to: ActorRef) extends SystemMessage
  case class StopForward(to: ActorRef) extends SystemMessage

  case class Bulk(reports: List[AgentReport]) extends SystemMessage
}


trait AgentReporting[Lang <: NegotiationLanguage] extends NegotiatingAgent[Lang] with SpeakingSystemSupport[Lang]{
  val reportingTo: Map[NegotiationId, AgentRef]

  protected def report(r: AgentReport) = reportingTo.get(r.negotiation) foreach (_.ref ! r)
}

object AgentReporting{

  trait AutoMessage[Lang <: NegotiationLanguage] extends AgentReporting[Lang] with AgentHelpers[Lang] with SystemSupport {
    protected def messageReportHook(to: AgentRef, msg: Lang#Msg) = { report(MessageReport(msg, to, extraMessage)); true }

    abstract override def process: PartialFunction[Lang#Msg, Any] = {
      case msg if super.process.isDefinedAt(msg) => hooks.OnSend.withHooks(messageReportHook)(super.process(msg))
    }

    abstract override def start(): Unit = hooks.OnSend.withHooks(messageReportHook)(super.start())
    abstract override def stop(): Unit = hooks.OnSend.withHooks(messageReportHook)(super.stop())
    abstract override def reset(): Unit = hooks.OnSend.withHooks(messageReportHook)(super.reset())

    def extraMessage: Map[String, Any]
  }
  
  trait StateByDemand[Lang <: NegotiationLanguage] extends AgentReporting[Lang]{
    self: ActorLogging =>

    def stateReport(negId: NegotiationId): StateReport
    
    override def processSys = super.processSys orElse{
      case AgentReport.StateRequest(negId) =>
        val s = sender()
        val rep = stateReport(negId)
        log.debug(s"StateRequest: s=$s, rep=$rep")
        s ! rep
    }
  }

  trait AutoState[Lang <: NegotiationLanguage] extends AgentReporting[Lang] {
    type Negotiation <: Negotiation.ChangeHooks
    
    for(neg <- negotiations){
      neg.ChangeHooks.add("state auto report", changes => report(StateReport(neg.id, changes, "changes")))
    }
  }
}

trait AutoReporting[Lang <: NegotiationLanguage] extends AgentReporting.AutoMessage[Lang]
  with AgentReporting.AutoState[Lang] with AgentReporting.StateByDemand[Lang]

case class ReportListenerRef[T <: ReportListener](clazz: Class[T], forward: List[ActorRef])

trait ReportListenerControllerSupport {
  self: NegotiationEnvironmentController =>

  trait ReportListenerBuilder[T <: ReportListener]{
    def build(clazz: Class[T]): AgentRef
  }

  protected val listeners = mutable.HashMap.empty[ReportListenerRef[_], AgentRef]

  def reportListener[R <: ReportListener](ref: ReportListenerRef[R])(implicit builder: ReportListenerBuilder[R]) =
    synchronized{
      listeners.getOrElseUpdate(ref, builder.build(ref.clazz))
    }

}

trait ReportListener extends SystemAgent{
  def reportReceived(r: AgentReport)

  def receive: Actor.Receive = {
    case r: AgentReport => reportReceived(r)
  }
}

trait NegotiationFinishedListener extends ReportListener with ActorLogging{
  log.info("NegotiationFinishedListener started")

  def controller: ActorRef
  def confirmAllWaitingDelay: FiniteDuration

  private lazy val _states = mutable.HashMap.empty[NegotiationId, mutable.Map[AgentRef, NegotiationState]]
  private lazy val _values = mutable.HashMap.empty[NegotiationId, mutable.Map[AgentRef, Map[Var, Any]]]

  def states = _states.toMap.mapValues(_.toMap)
  def values = _values.toMap.mapValues(_.toMap)

  def allWaiting(neg: NegotiationId) = states.get(neg).exists(_.forall(_._2 == NegotiationState.Waiting))

  def guardState(msg: StateReport) = {
    msg.state.get("state").map {
      state =>
        hadChanges = true
        _states.getOrElseUpdate(msg.negotiation, mutable.Map.empty) += msg.sender -> state.asInstanceOf[(NegotiationState, NegotiationState)]._2
    }
    msg.state.get("values").map{
      values =>
        hadChanges = true
        _values.getOrElseUpdate(msg.negotiation, mutable.Map.empty) += msg.sender -> values.asInstanceOf[(Map[Var, Any], Map[Var, Any])]._2
    }
  }

  private var hadChanges = false

  protected def allWaitingCheck(neg: NegotiationId) = if(allWaiting(neg)) {
    hadChanges = false
    context.system.scheduler
      .scheduleOnce(confirmAllWaitingDelay, self, "confirm all waiting" -> neg)(context.dispatcher)
  }

  override def receive = ({
    case r: StateReport =>
      guardState(r)
      super.receive(r)
      allWaitingCheck(r.negotiation)
    case ("confirm all waiting", neg: NegotiationId) => if(allWaiting(neg) && !hadChanges) negotiationFinished(neg)
  }: Actor.Receive) orElse super.receive

  def negotiationFinished(neg: NegotiationId) = {
    val vals = values(neg).map(_._2).toSeq
    controller ! SystemMessage.NegotiationFinished(neg, vals)
  }
}

trait ReportPrinter extends ReportListener{
  def print(r: AgentReport): String
}

trait ReportWriter extends ReportPrinter{
  val writeTo: File

  def reportReceived(r: AgentReport): Unit = {
    IOUtils.write(print(r), writer)
    writer.newLine()
    writer.flush()
  }

  private lazy val writer = new BufferedWriter(new FileWriter(writeTo))
}

trait ReportForwarder extends ReportListener{
  var forwardTo: Set[ActorRef] = Set()

  override def receive: Actor.Receive = super.receive orElse{
    case AgentReport.Forward(to)      => forwardTo += to
    case AgentReport.StopForward(to)  => forwardTo -= to
  }

  abstract override def reportReceived(r: AgentReport): Unit = {
    super.reportReceived(r)
    forwardTo foreach (_ ! r)
  }
}