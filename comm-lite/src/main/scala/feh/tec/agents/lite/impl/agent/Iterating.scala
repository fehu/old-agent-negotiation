package feh.tec.agents.lite.impl.agent

import feh.tec.agents.lite.ProposalEngine.DomainIterator
import feh.tec.agents.lite._
import feh.tec.agents.lite.impl.{ChangingIssues, DomainIterating}
import feh.tec.agents.lite.spec.AgentSpecification
import feh.util._

trait DomainIteratingAllVars[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityAndProposalBasedAgent[Lang] with DomainIterating[Lang]
{
  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang] with Negotiation.HasIterator
  type Agent <: PriorityAndProposalBasedAgent[Lang] with DomainIteratingAllVars[Lang]

  val varsByNeg: Map[NegotiationId, Set[Var]] = negotiationsInit.map(_.pair).toMap

  override val spec: AgentSpecification.PriorityAndProposalBased[Agent, Lang] with AgentSpecification.Iterating[Agent, Lang]

  def newIterator(neg: NegotiationId): DomainIterator = spec.newIterator.get apply neg
}

trait DomainIteratingChangingIssues[Lang <: Language.ProposalBased with Language.HasPriority with Language.NegotiatesIssues]
  extends PriorityAndProposalBasedAgent[Lang] with DomainIterating[Lang]
{
  agent: ChangingIssues[Lang] =>

  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang]
                       with Negotiation.ChangingIssues with Negotiation.HasIterators
  type Agent <: PriorityAndProposalBasedAgent[Lang] with DomainIteratingChangingIssues[Lang]
  
  val spec: AgentSpecification.PriorityAndProposalBased[Agent, Lang] with AgentSpecification.Iterating[Agent, Lang]

  def varsByNeg: Map[NegotiationId, Set[Var]] = negotiations.map(neg => neg.id -> neg.currentIssues().toSet).toMap

  def newIterator(neg: NegotiationId) = spec.newIterator.get apply neg
}