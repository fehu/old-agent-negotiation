package feh.tec.agents.light.impl

import akka.util.Timeout
import feh.tec.agents.light.impl.NegotiationEnvironmentController._
import feh.tec.agents.light._
import feh.tec.agents.light.spec.AgentProps.AgentPropsBundle
import feh.tec.agents.light.spec.NegotiationSpecification.{InterlocutorsByRoles, Interlocutors}
import scala.concurrent.{Await, Future}
import akka.pattern.ask

trait NegotiationEnvironmentController extends EnvironmentController with DynamicEnvironmentController{
  protected val initialAgents: Set[AgentPropsBundle[_]]
  protected val timeouts: Timeouts
  protected val systemAgentsProps: Set[AgentPropsBundle[_]]

  protected var state: NegotiationState = NegotiationState.Created
  protected var currentAgents: Set[AgentRef] = null
  protected var systemAgents: Set[AgentRef] = null

  implicit def asys = context.system

  def initialize(): Unit =
    if (state == NegotiationState.Created){
      state = NegotiationState.Initializing
      val (refs, negotiationInits) = initialAgents.map(b => b.create -> b.props.negotiationInits).unzip
      systemAgents = systemAgentsProps.map(_.create)
      currentAgents = refs
      initialAgentsCreated(refs.zip(negotiationInits.map(_.map(ext => ext.id -> ext.scope).toMap)).toMap)
      currentAgents foreach sendAndProcessAnswer(SystemMessage.Initialize, timeouts.initialize, _.mapTo[SystemMessage.Initialized.type])
      state = NegotiationState.Initialized
    }
    else wrongMessageForCurrentState(SystemMessage.Initialize)

  def start(): Unit =
    if (state == NegotiationState.Initialized || state == NegotiationState.Stopped) {
      state = NegotiationState.Starting
      currentAgents foreach sendAndProcessAnswer(SystemMessage.Start, timeouts.start, _.mapTo[SystemMessage.Started.type])
      state = NegotiationState.Negotiating
    }
    else wrongMessageForCurrentState(SystemMessage.Start)

  def stop(): Unit =
    if(state == NegotiationState.Negotiating){
      currentAgents foreach sendAndProcessAnswer(SystemMessage.Stop, timeouts.stop, _.mapTo[SystemMessage.Stopped.type])
      state = NegotiationState.Stopped
    }
    else wrongMessageForCurrentState(SystemMessage.Stop)

  def reset(): Unit =
    if(state == NegotiationState.Stopped){
      currentAgents foreach sendAndProcessAnswer(SystemMessage.Reset, timeouts.reset, _.mapTo[SystemMessage.Reset.type])
      state = NegotiationState.Initialized
    }
    else wrongMessageForCurrentState(SystemMessage.Reset)

  protected def sendAndProcessAnswer[R](msg: AbstractMessage, timeout: Timeout, f: Future[Any] => Future[R])(ag: AgentRef): R = {
    implicit def t = timeout
    Await.result(f(ag.ref ? msg), t.duration)
  }

  protected def newScope(ag: AgentRef, scope: Set[AgentRef], neg: NegotiationId) =
    ag.ref ! SystemMessage.ScopeUpdate.NewScope(scope, neg)

  def initialAgentsCreated(ag: Map[AgentRef, Map[NegotiationId, Interlocutors]]): Unit = ag foreach{
    case (ref, negotiations) => negotiations foreach{
      case (negId, InterlocutorsByRoles(roles)) =>
        val scope = currentAgents.filter(roles contains _.id.role.name)
        newScope(ref, scope, negId)
    }
  }

  private def wrongMessageForCurrentState(msg: SystemMessage) = sender() ! SystemMessage.UnexpectedCommand(msg, state)

  /** override me */
  def agentAdded(ag: AgentRef, neg: Set[NegotiationId]): Unit = ???
  /** override me */
  def agentRemoved(ag: AgentRef, neg: Set[NegotiationId]): Unit = ???

  def receive: Receive = {
    case SystemMessage.Initialize => initialize()
    case SystemMessage.Start      => start()
    case SystemMessage.Stop       => stop()
    case SystemMessage.Reset      => reset()
  }

  val name = NegotiationEnvironmentController.Name
  val role = NegotiationEnvironmentController.Role
}

object NegotiationEnvironmentController{
  trait Timeouts{
    def initialize: Timeout
    def start: Timeout
    def stop: Timeout
    def reset: Timeout
  }

  lazy val Name = "NegotiationEnvironmentController"
  lazy val Role = SystemRole(Name)
}