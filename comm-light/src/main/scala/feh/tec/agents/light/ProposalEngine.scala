package feh.tec.agents.light

trait ProposalEngine[Lang <: Language.ProposalBased] extends ProposalBasedAgent[Lang]{
  def nextValues(neg: NegotiationId): Option[Map[Var, Any]]
  def setNextProposal(neg: NegotiationId)
}
