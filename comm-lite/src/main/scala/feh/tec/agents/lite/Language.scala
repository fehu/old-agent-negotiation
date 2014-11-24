package feh.tec.agents.lite

import java.util.{Date, UUID}

trait Language{
  type Msg <: AnyRef
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
    type Msg = Message.HasPriority
    type Priority               = Message.HasPriority
  }

  trait HasPriorityNegotiation extends HasPriority{
    type PriorityRaiseRequest   <: Msg with Message.PriorityRaiseRequest[_]
    type PriorityRaiseResponse  <: Msg with Message.PriorityRaiseResponse
    type PriorityKeeping        = Message.Keep
    type PriorityRaising        = Message.Raise
  }

  trait NegotiatesIssues extends NegotiationLanguage{
    type IssueRequest   = Message.IssuesRequest
    type IssueResponse  = Message.IssuesDemand
  }
}

trait Message extends NegotiationMessage{
  val sender: AgentRef
  val negotiation: NegotiationId

  def asString: String

  override def toString = asString

}

object Message{
  trait HasPriority extends Message{
    def priority: Priority
  }

  case class ProposalId(id: UUID)
  object ProposalId{
    def rand = ProposalId(UUID.randomUUID())
  }
  
  case class Proposal(id: ProposalId, negotiation: NegotiationId, priority: Priority, get: Map[Var, Any])
                     (implicit val sender: AgentRef) extends Message.HasPriority{
    def asString = s"I would like to set values: $get, are you ok with it? ($priority)"
  }

  trait ProposalResponse extends Message.HasPriority{
    def accepted: Boolean
    def respondingTo: ProposalId
    def myValues: Map[Var, Any]
  }

  case class Accepted(negotiation: NegotiationId, respondingTo: ProposalId, priority: Priority, myValues: Map[Var, Any], waiting: Boolean)
                     (implicit val sender: AgentRef) extends ProposalResponse {
    def asString = s"I accept your offer ($priority)${if (waiting) " [Waiting]" else ""}"
    final def accepted = true
  }
  case class Rejected(negotiation: NegotiationId, respondingTo: ProposalId, priority: Priority, myValues: Map[Var, Any])
                     (implicit val sender: AgentRef) extends ProposalResponse {
    def asString = s"I reject your offer ($priority)"
    final def accepted = false
  }

  class PriorityRaiseRequestId{
    val initializedAt = new Date
    override def toString: String = s"PriorityRaiseRequestId(${initializedAt.getTime})"
  }
  object PriorityRaiseRequestId{
    def apply() = new PriorityRaiseRequestId
  }

  case class PriorityRaiseRequest[Ev](id: PriorityRaiseRequestId, negotiation: NegotiationId, priority: Priority, evidence: Ev)
                                     (implicit val sender: AgentRef) extends Message.HasPriority{
    def asString = s"I request my $priority to be raised"
  }

  trait PriorityRaiseResponse extends Message.HasPriority{
    def respondingTo: PriorityRaiseRequestId
  }

  case class Keep(negotiation: NegotiationId, respondingTo: PriorityRaiseRequestId, priority: Priority)
                 (implicit val sender: AgentRef) extends PriorityRaiseResponse{
    def asString = s"I will keep my $priority"
  }

  case class Raise(negotiation: NegotiationId, respondingTo: PriorityRaiseRequestId, priority: Priority)
                  (implicit val sender: AgentRef) extends PriorityRaiseResponse{
    def asString = s"I will raise my $priority"
  }
  
  case class IssuesRequest(negotiation: NegotiationId, req: IssueChange.Change, priority: Priority)
                         (implicit val sender: AgentRef) extends Message with HasPriority{
    def asString = s"I request to $req for $negotiation ($priority)"
  }
  
  object IssueChange{
    trait Change
    case class Add(preferences: Var*) extends Change { override def toString = "Add Issues " + preferences }
    case class Remove(preferences: Var*) extends Change{ override def toString = "Remove Issues " + preferences }
  }
  
  case class IssuesDemand(negotiation: NegotiationId, req: IssueChange.Change, priority: Priority)
                         (implicit val sender: AgentRef) extends Message with HasPriority{
    def asString = s"I demand you $req in $negotiation ($priority)"
  }
}
