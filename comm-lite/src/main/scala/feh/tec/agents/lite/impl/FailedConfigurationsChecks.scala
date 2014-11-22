package feh.tec.agents.lite.impl

import feh.tec.agents.lite.{PartialValuesConfiguration, Language}

trait FailedConfigurationsChecks[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityAndProposalBasedAgent[Lang]
{
//  type Agent <: PriorityAndProposalBasedAgent[Lang] with FailedConfigurationsChecks[Lang]

  /** yes / no / None = maybe */
  def repeatingAFailure(acceptance: Lang#Acceptance): Option[Boolean]
  def guardFailedConfiguration(failed: PartialValuesConfiguration)
}
