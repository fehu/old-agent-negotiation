package feh.tec.agents

import java.util.UUID

sealed trait AbstractMessage{
  def id: Message.Id
  def sender: AgentRef
}

trait Message extends AbstractMessage{
  def priority: Priority

  def asString: String

  override def toString = super.toString + "\n>  " + asString + "  <"

  /** The message's priority is higher than scope's
    */
  def ?>(implicit thatPriority: Priority) = this.priority.get > thatPriority.get
}

protected[agents] trait SystemMessage extends AbstractMessage

object SystemMessage{
  case class Start(id: Message.Id)(implicit val sender: AgentRef) extends SystemMessage

}

object Message{
  type Id = UUID

  trait AutoId{
    self: Message =>
    val id = UUID.randomUUID()
  }

  trait Request extends Message
  trait Response extends Message{
    def respondingTo: Message.Id
  }

  case class Accepted(offer: Message.Id)(implicit val sender: AgentRef, val priority: Priority)
    extends Response with AutoId
  {
    def respondingTo = offer
    def asString = "I accept your offer"
  }
  case class Rejected(offer: Message.Id)(implicit val sender: AgentRef, val priority: Priority)
    extends Response with AutoId
  {
    def respondingTo = offer
    def asString = "I reject your offer"
  }

  case class Proposal(get: Map[Var, Any])(implicit val sender: AgentRef, val priority: Priority)
    extends Request with AutoId
  {
    def asString = s"I would like to set values: $get, are you ok with it?"
  }

  abstract class Fallback(implicit val sender: AgentRef, val priority: Priority) extends Message {
    def asString = s"I ran out of possible variants, please fallback"
  }

  object Fallback{
    def apply(implicit sender: AgentRef, priority: Priority): Request with Fallback =
      new Fallback() with Request with AutoId
    def inResponse(to: Message.Id)(implicit sender: AgentRef, priority: Priority): Response with Fallback =
      new Fallback() with Response with AutoId{ def respondingTo = to }
  }

}
