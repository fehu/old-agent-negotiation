package feh.tec.agents

import java.util.UUID
import feh.util._
import feh.tec.agents.Message.{Response, AutoId}

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

  /** The message's priority is higher than scope's
    */
  def ?>(implicit thatPriority: Priority) = this.priority.get > thatPriority.get
}

protected[agents] trait SystemMessage extends AbstractMessage

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

  trait ScopeUpdate extends SystemMessage{
    def negotiation: NegotiationId
  }

  object ScopeUpdate{
    case class NewScope(scope: Set[AgentRef], negotiation: NegotiationId) extends ScopeUpdate with AutoId
    case class NewAgents(refs: Set[AgentRef], negotiation: NegotiationId) extends ScopeUpdate with AutoId
    case class RmAgents(refs: Set[AgentRef], negotiation: NegotiationId) extends ScopeUpdate with AutoId
  }

  case class RefDemand() extends SystemMessage with AutoId

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
    def asString = "I reject your offer"
  }

  trait AbstractProposal extends Request{
    def get: Map[Var, Any]
  }

  case class Proposal(negotiation: NegotiationId, get: Map[Var, Any])
                     (implicit val sender: AgentRef, val priority: Priority)
    extends AbstractProposal with AutoId
  {
    def messageType = "Proposal"
    def asString = s"I would like to set values: $get, are you ok with it?"
  }

  case class Demand(negotiation: NegotiationId, get: Map[Var, Any])
                   (implicit val sender: AgentRef, val priority: Priority)
    extends AbstractProposal with AutoId
  {
    def messageType = "Demand"
    def asString = s"I want you to set values: $get, do you accept?"
  }

  abstract class Fallback(val negotiation: NegotiationId)
                         (implicit val sender: AgentRef, val priority: Priority) extends Message {
    def messageType = "Fallback"
    def asString = s"I ran out of possible variants, please fallback"
  }

  object Fallback{
    def apply(negotiation: NegotiationId)(implicit sender: AgentRef, priority: Priority): Request with Fallback =
      new Fallback(negotiation) with Request with AutoId
    def inResponse(to: Message.Id, in: NegotiationId)(implicit sender: AgentRef, priority: Priority): Response with Fallback =
      new Fallback(in) with Response with AutoId{ def respondingTo = to }
  }

}
