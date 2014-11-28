package feh.tec.agents.lite.spec.macros.exp.impl.controller

import feh.tec.agents.lite.spec.macros.HasConstraintsBuilder
import feh.tec.agents.lite.spec.macros.exp.{AgentsBuildingMacroExperimentalBase, AgentsBuildingMacroExperimental, ControllerBuildingMacroExperimentalBase}

import scala.reflect.macros.whitebox

/**
 *
 */
trait ControllerBuildingMacroExperimentalImpl[C <: whitebox.Context] extends ControllerBuildingMacroExperimentalBase[C]
  with BeforeAgentsProps[C] with EmbedAgentProps[C] with AfterAgentsProps[C]
{
  self: AgentsBuildingMacroExperimentalBase[C] with HasConstraintsBuilder[C] =>

  def cBuilder: ConstraintsBuilder

  def ControllerSegmentsTransformation(raw: NegotiationRaw) =
    allBeforeAgentsProps(raw) ::: allEmbedAgentsProps(raw, cBuilder) ::: allAfterAgentsProps(raw)
}
