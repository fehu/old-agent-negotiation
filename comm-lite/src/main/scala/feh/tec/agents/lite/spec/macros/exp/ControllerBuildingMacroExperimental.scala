package feh.tec.agents.lite.spec.macros.exp

import feh.tec.agents.lite.spec.macros.{NegotiationBuildingMacro, ControllerBuildingMacro}

import scala.reflect.ClassTag
import scala.reflect.macros.whitebox

trait ControllerBuildingMacroExperimental[C <: whitebox.Context]
  extends ControllerBuildingMacroExperimentalEnvironment[C] with NegotiationBuildingMacro[C]{

  def ControllerSegmentsTransformation(negRaw: NegotiationRaw): List[MacroSegments => MacroSegments]

  def ControllerStagesOrdering: StagesOrdering //Ordering[MacroSegments.Stage]

//  def ControllerBuildSegments(negRaw: NegotiationRaw): List[MacroSegment] =
//    Function.chain(ControllerSegmentsTransformation(negRaw))(MacroSegments.empty(ControllerStagesOrdering)).segments

}

trait ControllerBuildingMacroExperimentalBase[C <: whitebox.Context] extends ControllerBuildingMacroExperimental[C] {

  object ControllerBuildingStages{
    case object AggregateParents extends MacroSegments.Stage
    case object EmbedIssues extends MacroSegments.Stage
    case object EmbedAgentProps extends MacroSegments.Stage
    case object EmbedExtraArgsValues extends MacroSegments.Stage
    case object EmbedSpawnsAndTimeouts extends MacroSegments.Stage
    case object EmbedControllerDefinitions extends MacroSegments.Stage
    case object Extra extends MacroSegments.Stage
  }

  lazy val ControllerStagesOrdering = {
    import ControllerBuildingStages._
    import StagesOrdering._
    StagesOrdering(
      AggregateParents >>: EmbedIssues >>: EmbedAgentProps >>: EmbedExtraArgsValues >>: EmbedSpawnsAndTimeouts
        >>: EmbedControllerDefinitions >>: Extra
    )
  }
}