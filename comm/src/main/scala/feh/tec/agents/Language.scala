package feh.tec.agents

import feh.tec.agents.Message.Response

trait Language {
  type Msg <: Message
  
  def isMessage(any: Any): Boolean
}

trait ConflictLanguage extends Language{
  type Conflict <: Msg
}

trait ProposalLanguage extends ConflictLanguage{
  type Proposal <: Msg
  type Rejected <: Msg with Response
  type Accepted <: Msg with Response
  
  def isProposal(msg: Any): Boolean
  def isRejection(msg: Any): Boolean
  def isAcceptance(msg: Any): Boolean
}

trait CounterProposalLanguage extends ProposalLanguage{
  type CounterProposal <: Rejected

  def isCounterProposal(msg: Any): Boolean
}

trait DataExtractor[Lang <: Language]{
  type Tpe
  def extract: PartialFunction[Message, Tpe]
}

trait IssuesExtractor[Lang <: Language] extends DataExtractor[Lang]{
  type Tpe = Map[Var, Any]
}