package feh.tec.agents.lite.spec.macros.exp

import feh.tec.agents.lite.spec.macros.{ControllerBuildingMacro, AgentsBuildingMacro}

import scala.reflect.macros.whitebox

trait AgentsBuildingMacroExperimental[C <: whitebox.Context] extends AgentsBuildingMacro[C]{
  self: ControllerBuildingMacroExperimental[C] =>

  def SegmentsTransformation(negRaw: NegotiationRaw): List[MacroSegments => MacroSegments]

  def stagesOrder: Ordering[MacroSegments.Stage]

  def AgentBuildSegments(negRaw: NegotiationRaw): List[MacroSegment] =
    Function.chain(SegmentsTransformation(negRaw))(MacroSegments.empty(stagesOrder)).segments

  object AgentBuildingStages{
    def BuildingBodies = MacroSegments.Stage.AgentMain
    case object AggregatingParents extends MacroSegments.Stage//(BuildingBodies.priority - 100)
  }

}
