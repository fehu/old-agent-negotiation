package feh.tec.agents.lite.spec.macros.exp

import feh.tec.agents.lite.spec.macros.{NegotiationBuildingMacro, AgentsBuildingMacro}
import feh.util._

import scala.reflect.macros.whitebox

trait AgentsBuildingMacroExperimental[C <: whitebox.Context]
  extends ControllerBuildingMacroExperimentalEnvironment[C] with NegotiationBuildingMacro[C]
{
  def AgentSegmentsTransformation(negRaw: NegotiationRaw): List[MacroSegments => MacroSegments]

  def agentStagesOrder: Ordering[MacroSegments.Stage]

  def AgentBuildSegments(negRaw: NegotiationRaw): List[MacroSegment] =
    Function.chain(AgentSegmentsTransformation(negRaw))(MacroSegments.empty(agentStagesOrder)).segments
}


trait AgentsBuildingMacroExperimentalBase[C <: whitebox.Context] extends AgentsBuildingMacroExperimental[C]{
  self: ControllerBuildingMacroExperimental[C] =>

  object AgentBuildingStages{
    case object CreateAgentTrees extends MacroSegments.Stage
    case object AggregatingParents extends MacroSegments.Stage
    case object TypesDefinitions extends MacroSegments.Stage
    case object ValAndDefDefinitions extends MacroSegments.Stage
    case object ConstructorDefinition extends MacroSegments.Stage
  }

  lazy val agentStagesOrder: Ordering[MacroSegments.Stage] = {
    import AgentBuildingStages._
    import StagesOrdering._
    StagesOrdering(
      CreateAgentTrees >>: AggregatingParents >>: TypesDefinitions >>: ValAndDefDefinitions >>: ConstructorDefinition
    )
  }

  type RequiredAgentArgs = Map[String, Map[String, (c.Type, c.Tree)]]

  protected def transform(in: List[Raw.AgentDef])(f: (ActorTrees, Raw.AgentDef) => ActorTrees): I[(AgentName, ActorTrees)] =
    original => in.find(_.name == original._1).map(raw => original._1 -> f(original._2, raw)) getOrElse original

  protected def agentArgsRequired(in: MacroSegments): RequiredAgentArgs =
    in.getExtra[RequiredAgentArgs]("args-required") getOrElse Map()

  protected case class AddAgentArgs(agentName: String, argName: String, argType: c.Type, argTree: c.Tree)

  protected implicit class MacroSegmentsWrapper(ms: MacroSegments) {
    def addAgentArgs(addArgs: Seq[AddAgentArgs]*): MacroSegments = ms.changeExtra[RequiredAgentArgs]("args-required", _.map{
      args =>
        args ++ addArgs.flatMap(_.map{
          case AddAgentArgs(agentName, argName, argType, argTree) =>
            agentName -> (args.getOrElse(agentName, Map()) + (argName ->(argType, argTree)))
        })
    })
  }
}
