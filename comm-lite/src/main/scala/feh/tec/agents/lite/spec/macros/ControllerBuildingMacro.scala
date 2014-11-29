package feh.tec.agents.lite.spec.macros

import akka.actor.Props
import feh.tec.agents.lite.spec

import scala.reflect.macros.whitebox

trait ControllerBuildingMacro[C <: whitebox.Context]
  extends ControllerBuildingMacroEnvironment[C] with NegotiationBuildingMacro[C]{

  def ControllerSegmentsTransformation(negRaw: NegotiationRaw): List[MacroSegments => MacroSegments]

  def ControllerStagesOrdering: StagesOrdering //Ordering[MacroSegments.Stage]

}

trait StandardControllerBuildingMacroInterface[C <: whitebox.Context]{
  self: ControllerBuildingMacro[C] =>

  def controllerPropsExpr(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[Props]
}


trait ControllerBuildingMacroBase[C <: whitebox.Context] extends ControllerBuildingMacro[C]
  with StandardControllerBuildingMacroInterface[C]
{

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