package feh.tec.agents.light.impl.agent

import feh.tec.agents.light.ProposalEngine.DomainIterator
import feh.tec.agents.light._
import feh.tec.agents.light.impl.DomainIterating
import feh.tec.agents.light.spec.AgentSpecification
import feh.util._

trait DomainIteratingAllVars[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityAndProposalBasedAgent[Lang] with DomainIterating[Lang]
{
  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang] with Negotiation.HasIterator

  override val spec: AgentSpecification.PriorityAndProposalBased[this.type, Lang] with AgentSpecification.Iterating[this.type, Lang]

  def newIterator(neg: NegotiationId): DomainIterator = spec.newIterator.get apply neg
}
