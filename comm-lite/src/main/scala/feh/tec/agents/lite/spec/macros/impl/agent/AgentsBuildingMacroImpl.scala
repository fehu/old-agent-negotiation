package feh.tec.agents.lite.spec.macros.impl.agent

import feh.tec.agents.lite.spec.macros.{ControllerBuildingMacro, AgentsBuildingMacroBase}
import scala.reflect.macros.whitebox

trait AgentsBuildingMacroImpl[C <: whitebox.Context] extends AgentsBuildingMacroBase[C]
  with CreateAgentTrees[C]
  with AggregateParents[C]
  with TypesDefinitions[C]
  with ValAndDefDefinitions[C]
{
  self: ControllerBuildingMacro[C] =>

  def AgentSegmentsTransformation(raw: NegotiationRaw) =
    allCreateAgentTrees(raw) ::: allAggregatingParents(raw) ::: allTypesDefinitions ::: allValAndDefDefinitions(raw)
}
