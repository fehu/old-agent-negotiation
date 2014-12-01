package feh.tec.agents.lite

trait FailedPartialSolutionsChecks[Lang <: Language.ProposalBased with Language.HasPriority with Language.NegotiatesIssues]
  extends PriorityProposalBasedAgent[Lang] with ChangingIssues[Lang]
{
  type Negotiation <: Negotiation.HasPriority with Negotiation.HasProposal[Lang] with Negotiation.ChangingIssues

  /** yes / no / None = maybe */
  def repeatingAFailure(ps: PartialSolution): Boolean
  def guardFailedPartialSolution(failed: PartialSolution)
}
