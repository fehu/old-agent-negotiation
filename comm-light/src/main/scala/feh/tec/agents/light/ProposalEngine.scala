package feh.tec.agents.light

trait ProposalEngine[Lang <: Language.ProposalBased] extends ProposalBasedAgent[Lang]{
  def nextValues(neg: NegotiationId): Option[Map[Var, Any]]
  def setNextProposal(neg: NegotiationId)
}

object ProposalEngine{

  trait Iterating[Lang <: Language.ProposalBased] extends ProposalEngine[Lang]{
    type DomainIterator = Iterator[Map[Var, Any]]
    type Negotiation <: Negotiation.HasIterator[DomainIterator]

    def newIterator(neg: NegotiationId): DomainIterator
  }

  trait Filtering[Lang <: Language.ProposalBased] extends ProposalEngine[Lang] {
    self: Iterating[Lang] =>

    def filterIterator: DomainIterator => DomainIterator
  }

}