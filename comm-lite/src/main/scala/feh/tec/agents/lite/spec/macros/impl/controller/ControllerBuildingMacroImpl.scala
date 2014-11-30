package feh.tec.agents.lite.spec.macros.impl.controller

import feh.tec.agents.lite.spec.macros.{AgentsBuildingMacroBase, ControllerBuildingMacroBase, HasConstraintsBuilder}

import scala.reflect.macros.whitebox

/**
 *
 */
trait ControllerBuildingMacroImpl[C <: whitebox.Context] extends ControllerBuildingMacroBase[C]
  with BeforeAgentsProps[C] with EmbedAgentProps[C] with AfterAgentsProps[C]
{
  self: AgentsBuildingMacroBase[C] with HasConstraintsBuilder[C] =>

  def cBuilder: ConstraintsBuilder

  def ControllerSegmentsTransformation(raw: NegotiationRaw) =
    allBeforeAgentsProps(raw) ::: allEmbedAgentsProps(raw, cBuilder) ::: allAfterAgentsProps(raw)
}
