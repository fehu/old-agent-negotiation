package feh.tec.agents.lite.spec.macros.exp.impl

import feh.tec.agents.lite.spec.macros.exp.{AgentsBuildingMacroExperimentalBase, ControllerBuildingMacroExperimental}

import scala.reflect.macros.whitebox

trait AgentsBuildingMacroExperimentalImpl[C <: whitebox.Context] extends AgentsBuildingMacroExperimentalBase[C]
  with CreateAgentTrees[C]
  with AggregatingParents[C]
{
  self: ControllerBuildingMacroExperimental[C] =>

  def SegmentsTransformation(raw: NegotiationRaw) =
    allCreateAgentTrees(raw) ::: allAggregatingParents(raw)
}
