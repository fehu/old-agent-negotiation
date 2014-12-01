package feh.tec.agents.lite

trait FailedConfigurationsChecks[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityProposalBasedAgent[Lang]
{
//  type Agent <: PriorityAndProposalBasedAgent[Lang] with FailedConfigurationsChecks[Lang]

  /** yes / no / None = maybe */
  def repeatingAFailure(acceptance: Lang#Acceptance): Option[Boolean]
  def guardFailedConfiguration(failed: PartialValuesConfiguration)
}
