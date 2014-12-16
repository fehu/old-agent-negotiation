package feh.tec.agents.lite

import feh.tec.agents.lite.DomainIteratorBuilder.LinkedDomainIterator

trait ProposalEngine[Lang <: Language.ProposalBased] extends ProposalBasedAgent[Lang]{
  def nextValues(neg: NegotiationId): Option[Map[Var, Any]]
  def setNextProposal(neg: NegotiationId): Lang#Proposal
}

object ProposalEngine{
  type DomainIterator = LinkedDomainIterator[Map[Var, Any]]

  trait Iterating[Lang <: Language.ProposalBased] extends ProposalEngine[Lang]{
    type Negotiation <: Negotiation.HasProposal[Lang] with Negotiation.HasIterator

    def newIterator(neg: NegotiationId): DomainIterator
  }

  trait Filtering[Lang <: Language.ProposalBased] extends ProposalEngine[Lang] {
    self: Iterating[Lang] =>

    def filterIterator: DomainIterator => DomainIterator
  }

}