package feh.tec.agents

trait Language {
  type Msg <: Message
  
  def isMessage(any: Any): Boolean
}

trait ProposalLanguage extends Language{
  type Proposal <: Msg
  type Rejected <: Msg
  type Accepted <: Msg
  
  def isProposal(msg: Any): Boolean
  def isRejection(msg: Any): Boolean
  def isAcceptance(msg: Any): Boolean
}

trait CounterProposalLanguage extends ProposalLanguage{
  type CounterProposal <: Rejected

  def isCounterProposal(msg: Any): Boolean
}

trait BacktrackLanguage extends ProposalLanguage{
  type Fallback <: Msg

  def isFallback(msg: Any): Boolean
}

object Language{
/*
  trait Priority{
    self: Language =>

    type Msg <: {
      def priority: Priority
    }
  }
*/
}