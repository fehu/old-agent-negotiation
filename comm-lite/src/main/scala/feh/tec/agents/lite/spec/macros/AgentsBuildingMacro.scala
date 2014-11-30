package feh.tec.agents.lite.spec.macros

import feh.tec.agents.lite.spec.NegotiationSpecification
import feh.util._

import scala.reflect.macros.whitebox

trait AgentsBuildingMacro[C <: whitebox.Context]
  extends ControllerBuildingMacroEnvironment[C] with NegotiationBuildingMacro[C]
{
  def AgentSegmentsTransformation(negRaw: NegotiationRaw): List[MacroSegments => MacroSegments]

  def AgentStagesOrdering: StagesOrdering

}


trait AgentsBuildingMacroBase[C <: whitebox.Context] extends AgentsBuildingMacro[C]{

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
      val byAgName = addArgs.flatten.groupBy(_.agentName)

      def argsToAdd(args: RequiredAgentArgs): RequiredAgentArgs = byAgName map  {
        case (name, add) =>
          val agMap = args.getOrElse(name, Map())
          val toAdd = add.map{ case AddAgentArgs(agentName, argName, argType, argTree) => argName ->(argType, argTree) }
         name -> (agMap ++ toAdd)
      }

      trees.changeExtra[RequiredAgentArgs]("args-required", opt =>
        Some{
          opt
            .map {args => args ++ argsToAdd(args)}
            .getOrElse(argsToAdd(Map()).toMap)
        }
      )
    }
  }
}

trait HasConstraintsBuilder[C <: whitebox.Context] extends NegotiationBuildingMacro[C] {

  trait ConstraintsBuilder{
    def build(agDef: Raw.AgentConstraintsDef, raw: NegotiationRaw): c.Expr[NegotiationSpecification.AgentConstraintsDef]
  }

}

