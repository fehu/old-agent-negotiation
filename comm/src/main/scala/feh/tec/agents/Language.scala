package feh.tec.agents

trait Language {
  type Msg
}

trait ProposalLanguage extends Language{
  type Proposal <: Msg
  type Rejected <: Msg
  type Accepted <: Msg
}

trait CounterProposalLanguage extends ProposalLanguage{
  type CounterProposal <: Rejected
}

trait BacktrackLanguage extends ProposalLanguage{
  type Fallback <: Msg
}

object Language{
  trait Priority{
    self: Language =>

    type Msg <: {
      def priority: Priority
    }
  }
}