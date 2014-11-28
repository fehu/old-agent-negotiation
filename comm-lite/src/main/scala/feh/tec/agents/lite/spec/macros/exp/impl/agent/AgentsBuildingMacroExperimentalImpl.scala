package feh.tec.agents.lite.spec.macros.exp.impl.agent

import feh.tec.agents.lite.spec.macros.exp.{AgentsBuildingMacroExperimentalBase, ControllerBuildingMacroExperimental}

import scala.reflect.macros.whitebox

trait AgentsBuildingMacroExperimentalImpl[C <: whitebox.Context] extends AgentsBuildingMacroExperimentalBase[C]
  with CreateAgentTrees[C]
  with AggregatingParents[C]
  with TypesDefinitions[C]
{
  self: ControllerBuildingMacroExperimental[C] =>

  def AgentSegmentsTransformation(raw: NegotiationRaw) =
    allCreateAgentTrees(raw) ::: allAggregatingParents(raw) ::: allTypesDefinitions
}
