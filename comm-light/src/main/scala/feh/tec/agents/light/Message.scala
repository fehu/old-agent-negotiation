package feh.tec.agents.light

sealed trait AbstractMessage

trait SystemMessage extends AbstractMessage
trait NegotiationMessage extends AbstractMessage
trait UserMessage extends AbstractMessage


object SystemMessage{
  case object RefDemand extends SystemMessage

  case object Start extends SystemMessage
  case object Started extends SystemMessage

  case object Stop extends SystemMessage
  case object Stopped extends SystemMessage

  case object Reset extends SystemMessage

  trait ScopeUpdate extends SystemMessage{
    def negotiation: NegotiationId
  }

  object ScopeUpdate{
    case class NewScope(scope: Set[AgentRef], negotiation: NegotiationId) extends ScopeUpdate
    case class NewAgents(refs: Set[AgentRef], negotiation: NegotiationId) extends ScopeUpdate
    case class RmAgents(refs: Set[AgentRef], negotiation: NegotiationId) extends ScopeUpdate
  }


  case class NegotiationFinished(neg: NegotiationId) extends SystemMessage
}