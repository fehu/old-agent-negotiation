package feh.tec.agents.lite.spec.macros.exp

import feh.tec.agents.lite.spec.macros.{NegotiationBuildingMacro, AgentsBuildingMacro}
import feh.util._

import scala.collection.mutable
import scala.reflect.macros.whitebox

trait AgentsBuildingMacroExperimental[C <: whitebox.Context]
  extends ControllerBuildingMacroExperimentalEnvironment[C] with NegotiationBuildingMacro[C]
{
  def AgentSegmentsTransformation(negRaw: NegotiationRaw): List[MacroSegments => MacroSegments]

  def AgentStagesOrdering: StagesOrdering // Ordering[MacroSegments.Stage]

//  def AgentBuildSegments(negRaw: NegotiationRaw): List[MacroSegment] =
//    Function.chain(AgentSegmentsTransformation(negRaw))(MacroSegments.empty(AgentStagesOrdering)).segments
}


trait AgentsBuildingMacroExperimentalBase[C <: whitebox.Context] extends AgentsBuildingMacroExperimental[C]{

  object AgentBuildingStages{
    case object CreateAgentTrees extends MacroSegments.Stage
    case object AggregateParents extends MacroSegments.Stage
    case object TypesDefinitions extends MacroSegments.Stage
    case object ValAndDefDefinitions extends MacroSegments.Stage
    case object ConstructorDefinition extends MacroSegments.Stage
  }

  lazy val AgentStagesOrdering = {
    import AgentBuildingStages._
    import StagesOrdering._
    StagesOrdering(
      CreateAgentTrees >>: AggregateParents >>: TypesDefinitions >>: ValAndDefDefinitions >>: ConstructorDefinition
    )
  }

  type RequiredAgentArgs = Map[String, Map[String, (c.Type, c.Tree)]]

  protected def transform(in: List[Raw.AgentDef])(f: (ActorTrees, Raw.AgentDef) => ActorTrees): I[(AgentName, ActorTrees)] =
    original => in.find(_.name == original._1).map(raw => original._1 -> f(original._2, raw)) getOrElse original

  protected def agentArgsRequired(in: Trees): RequiredAgentArgs =
    in.getExtra[RequiredAgentArgs]("args-required") getOrElse Map()

  protected case class AddAgentArgs(agentName: String, argName: String, argType: c.Type, argTree: c.Tree)

  protected implicit class TreesWrapper(trees: Trees) {
    def addAgentArgs(addArgs: Seq[AddAgentArgs]*): Trees = {

      def argsToAdd(args: RequiredAgentArgs) = addArgs.flatMap(_.map{
        case AddAgentArgs(agentName, argName, argType, argTree) =>
          agentName -> (args.getOrElse(agentName, Map()) + (argName ->(argType, argTree)))
      })

      trees.changeExtra[RequiredAgentArgs]("args-required", opt =>
        Some{
          opt
            .map {args => args ++ argsToAdd(args)}
            .getOrElse(argsToAdd(Map()).toMap)
        }
      )
    }
  }

  /*
    def addAgentArgs(addArgs: Seq[AddAgentArgs]*): MacroSegments = ms.changeExtra[RequiredAgentArgs]("args-required", _.map{
      args =>
        args ++ {
          val b = mutable.Map.empty[String, Map[String, (c.Type, c.Tree)]]
          addArgs.foreach(_.map{
            case AddAgentArgs(agentName, argName, argType, argTree) =>
              b += agentName -> (b.orElse(args).applyOrElse(agentName, (_: Any) => Map.empty[String, (c.Type, c.Tree)]) + (argName ->(argType, argTree)))
          })
          b.result()
        }
    })
   */
}
