package feh.tec.agents.lite.impl

import feh.tec.agents.lite.{PartialValuesConfiguration, Language, NegotiatingAgent}

trait FailedConfigurationsChecks[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityAndProposalBasedAgent[Lang]
{
  /** yes / no / None = maybe */
  def repeatingAFailure(acceptance: Lang#Acceptance): Option[Boolean]
  def guardFailedConfiguration(failed: PartialValuesConfiguration)
}
