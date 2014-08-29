package feh.tec.agents.impl.agent

import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents._
import feh.tec.agents.impl.AgentCreation.NegotiationInit
import feh.tec.agents.impl.Language.Buildable
import feh.tec.agents.impl.view.{ConstraintsSatisfactionWithPriority, Constraints}
import feh.tec.agents.impl.{AgentCreation, PriorityBasedAgentViews, PriorityBasedAgent, Agent}



trait PriorityBased[Lang <: ProposalLanguage] extends AgentCreation[Lang] with PriorityBasedAgent[Lang] with PriorityBasedAgentViews{

  lazy val constraintsSatisfactions = ConstraintsSatisfactionWithPriority(lang)
  lazy val proposalSatisfaction = new Constraints(constraints)

  def accept(prop: Lang#Proposal) = issuesExtractor.extract(prop) forall (proposalSatisfaction.satisfies _).tupled

  // ConstraintsCheckPolicy mechanism
  def checkConstraints() = ???

  // trait ProposalRegistering
  def expectingResponse(id: Message.Id) = ???

  // trait NegotiationCreation
  type ANegotiation = Nothing
  def createNegotiation(id: NegotiationId, init: NegotiationInit) = ???

  // various mix-ins
  val lang: Lang = ???
  def buildMessage(negotiation: NegotiationId, b: Buildable) = ???

  // definition
  val role: Role = ???
  val vars: Set[Var] = ???
  def constraints: Set[Constraint[Var]]

  // init
  def conflictResolver = ???
  def conflictResolveTimeout = ???
  protected def issuesExtractor: IssuesExtractor[Lang]

}

