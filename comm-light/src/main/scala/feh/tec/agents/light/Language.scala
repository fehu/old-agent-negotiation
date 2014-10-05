package feh.tec.agents.light

import java.util.UUID

trait Language{
  type Msg
}

trait NegotiationLanguage extends Language{
  type Msg <: Message
}

object Language{
  trait ProposalBased extends NegotiationLanguage{
    type Proposal   = Message.Proposal
    type Acceptance = Message.Accepted
    type Rejection  = Message.Rejected
  }

  trait HasPriority extends NegotiationLanguage{
    type Msg <: Message.HasPriority
    
    trait Priority{
      type RaiseRequest <: Message.PriorityRaiseRequest[_]
      type Keeping      = Message.Keep
      type Raising      = Message.Raise
    }
  }
}

trait Message extends NegotiationMessage{
  def sender: AgentRef
  def negotiation: NegotiationId

  def asString: String

  override def toString = asString

}

object Message{
  trait HasPriority extends Message{
    def priority: Priority
  }

  case class ProposalId(get: UUID)
  
  case class Proposal(id: ProposalId, negotiation: NegotiationId, get: Map[Var, Any])
                     (implicit val sender: AgentRef, val priority: Priority) extends Message.HasPriority{
    def asString = s"I would like to set values: $get, are you ok with it?"
  }

  trait ProposalResponse extends Message.HasPriority{
    def respondingTo: ProposalId
    def myValues: Map[Var, Any]
  }

  case class Accepted(negotiation: NegotiationId, respondingTo: ProposalId, myValues: Map[Var, Any])
                     (implicit val sender: AgentRef, val priority: Priority) extends ProposalResponse {
    def asString = "I accept your offer"
  }
  case class Rejected(negotiation: NegotiationId, respondingTo: ProposalId, myValues: Map[Var, Any])
                     (implicit val sender: AgentRef, val priority: Priority) extends ProposalResponse {
    def asString = "I reject your offer"
  }

  case class PriorityRaiseRequestId(get: UUID)

  case class PriorityRaiseRequest[Ev](id: PriorityRaiseRequestId, negotiation: NegotiationId, evidence: Ev)
                                     (implicit val sender: AgentRef, val priority: Priority) extends Message.HasPriority{
    def asString = s"I request my $priority to be raised"
  }

  trait PriorityRaiseResponse extends Message.HasPriority{
    def respondingTo: PriorityRaiseRequestId
  }

  case class Keep(negotiation: NegotiationId, respondingTo: PriorityRaiseRequestId)
                 (implicit val sender: AgentRef, val priority: Priority) extends PriorityRaiseResponse{
    def asString = s"I will keep my $priority"
  }

  case class Raise(negotiation: NegotiationId, respondingTo: PriorityRaiseRequestId)
                  (implicit val sender: AgentRef, val priority: Priority) extends PriorityRaiseResponse{
    def asString = s"I will raise my $priority"
  }
}
