package feh.tec.agents

import java.util.UUID

import feh.tec.agents.Message.AutoId

import scala.concurrent.duration.Duration

sealed trait AbstractMessage{
  def id: Message.Id
}

trait Message extends AbstractMessage{
  def messageType: String

  def priority: Priority

  def sender: AgentRef

  def negotiation: NegotiationId

  def asString: String

  override def toString = s"$messageType: $asString (${sender.id}, $priority})"

}

protected[agents] trait SystemMessage extends AbstractMessage
protected[agents] abstract class LocalSystemMessage extends SystemMessage{ def id = ??? }

object SystemMessage{
  case class Start() extends SystemMessage with AutoId{

    def done = Start.Started(id)
    def notInitialized = Start.NotInitialized(id)
    def stillInitializing = Start.StillInitializing(id)
    def alreadyRunning = Start.AlreadyRunning(id)
  }
  object Start{
    case class Started protected[Start] (respondingTo: Message.Id) extends SystemMessage { def id = respondingTo }
    case class NotInitialized protected[Start] (respondingTo: Message.Id) extends SystemMessage { def id = respondingTo }
    case class StillInitializing protected[Start] (respondingTo: Message.Id) extends SystemMessage { def id = respondingTo }
    case class AlreadyRunning protected[Start] (respondingTo: Message.Id) extends SystemMessage { def id = respondingTo }
  }

  case class Stop() extends SystemMessage with AutoId{
    def stopped = Stopped(id)
  }
  case class Stopped(respondingTo: Message.Id)  extends SystemMessage { def id = respondingTo }

  case class Resume() extends SystemMessage with AutoId{
    def resumed = Resumed(id)
  }
  case class Resumed(respondingTo: Message.Id) extends SystemMessage { def id = respondingTo }

  trait ScopeUpdate extends SystemMessage{
    def negotiation: NegotiationId
  }

  object ScopeUpdate{
    case class NewScope(scope: Set[AgentRef], negotiation: NegotiationId) extends ScopeUpdate with AutoId
    case class NewAgents(refs: Set[AgentRef], negotiation: NegotiationId) extends ScopeUpdate with AutoId
    case class RmAgents(refs: Set[AgentRef], negotiation: NegotiationId) extends ScopeUpdate with AutoId
  }

  case class RefDemand() extends SystemMessage with AutoId

  case class NegotiationFinished(neg: NegotiationId) extends SystemMessage with AutoId
  case class NegotiationFinishedAutoRestart(neg: NegotiationId, restartingIn: Duration) extends SystemMessage with AutoId
}

object Message{
  type Id = UUID

  trait AutoId{
    self: AbstractMessage =>
    val id = UUID.randomUUID()
  }

  trait Request extends Message
  trait Response extends Message{
    def respondingTo: Message.Id
  }
  trait Conflict extends Message{
    def messageType = "Conflict"
  }

  case class Accepted(negotiation: NegotiationId, offer: Message.Id)
                     (implicit val sender: AgentRef, val priority: Priority)
    extends Response with AutoId
  {

    def messageType = "Accepted"
    def respondingTo = offer
    def asString = "I accept your offer"
  }
  case class Rejected(negotiation: NegotiationId, offer: Message.Id)
                     (implicit val sender: AgentRef, val priority: Priority)
    extends Response with AutoId
  {
    def messageType = "Rejected"
    def respondingTo = offer
    def asString = s"I reject your offer $offer"
  }

  trait AbstractProposal extends Request{
    def get: Map[Var, Any]
  }

  case class Proposal(negotiation: NegotiationId, get: Map[Var, Any])
                     (implicit val sender: AgentRef, val priority: Priority)
    extends AbstractProposal with AutoId
  {
    def messageType = "Proposal"
    def asString = s"I would like to set values: $get, are you ok with it? ($id)"
  }

  case class Demand(negotiation: NegotiationId, get: Map[Var, Any])
                   (implicit val sender: AgentRef, val priority: Priority)
    extends AbstractProposal with AutoId
  {
    def messageType = "Demand"
    def asString = s"I want you to set values: $get, do you accept?"
  }

  case class PriorityConflict(negotiation: NegotiationId)
                             (implicit val sender: AgentRef, val priority: Priority) extends Conflict with AutoId
  {
    def asString = "I'm in conflict with your priority, address the conflict resolver!"
  }

}
