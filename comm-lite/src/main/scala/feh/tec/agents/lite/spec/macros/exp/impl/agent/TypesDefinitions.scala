package feh.tec.agents.lite.spec.macros.exp.impl.agent

import feh.tec.agents.lite.spec.macros.exp.{AgentsBuildingMacroExperimentalBase, ControllerBuildingMacroExperimental}
import feh.tec.agents.lite.{Language, SystemAgent}
import feh.util._

import scala.reflect.macros.whitebox

/**
 * Contains `MacroSegmentsTransform`s for `TypesDefinitions` stage
 */
trait TypesDefinitions[C <: whitebox.Context]{
  self: AgentsBuildingMacroExperimentalBase[C] =>

  import c.universe._

  def allTypesDefinitions = AgentSegmentLangDefinition :: AgentSegmentTypesDefinitions :: Nil

//  protected def agentLang(in: MacroSegments, agentName: String): Option[c.Type] =
//    in.getExtra[Map[String, c.Type]]("agent-lang").flatMap(_.get(agentName))
//
//  protected[TypesDefinitions] def setAgentLang(in: MacroSegments)(agentName: String, lang: c.Type) =
//    in.changeExtra[Map[String, c.Type]]("agent-lang", _.map{ _ + (agentName -> lang) })

  protected def discomposeTypesAndBounds(from: c.Type) = from match {
    case TypeBounds(_, RefinedType(tpes, _)) => tpes
    case TypeBounds(_, ref: TypeRef) => ref :: Nil
    case RefinedType(tpes, _) => tpes
    case ref: TypeRef => ref :: Nil
  }

  protected def unitedType(of: List[c.Type]) = internal.refinedType(of, internal.newScopeWith())

  def langTypeName(anonAgentClassName: String) = TypeName(anonAgentClassName + "Lang")

  def AgentSegmentLangDefinition = MacroSegmentsTransform{
    _.append(AgentBuildingStages.TypesDefinitions,
      MacroSegment{
        case trees@Trees(_, agents) =>
          val newAgents = agents.map{
            case (name, tr@ActorTrees(cName, parents, _, _)) =>
              val langCompTypes = parents.flatMap(_.typeArgs.filter(_.resultType <:< typeOf[Language])) flatMap discomposeTypesAndBounds
              val langType = unitedType(langCompTypes)

              name -> tr.prepend.body(q"type ${langTypeName(cName)} = $langType")
          }

          trees.copy(agents = newAgents)
      }
    )
  }

  def AgentSegmentTypesDefinitions = MacroSegmentsTransform(
    _.append(AgentBuildingStages.TypesDefinitions,
      MacroSegment{
        case trees@Trees(_, ags) =>
          import c.universe._

          val newAgs = ags.map{
            case (name, tr@ActorTrees(_, parents, _, _)) if !parents.exists(_ <:< typeOf[SystemAgent]) =>
              val abstractTypeMembers = parents
                .flatMap(
                  _.members
                    .withFilter(d => d.name.isTypeName && d.isAbstract)
                    .map(
                      sym =>
                        sym.asType.name ->
                          (sym.typeSignature match {
                            case TypeBounds(_, RefinedType(tpes, _)) => tpes
                            case TypeBounds(_, ref: TypeRef) => ref :: Nil
                            case _ => Nil
                          })
                    )
                ).groupBy(_._1)
                .mapValues(_.unzip._2.flatten.distinctBy(_.typeSymbol.name))
                .filter(_._2.nonEmpty)

              val typeDefs = abstractTypeMembers
                //            .mapValues(_.map(replaceLanguageTypeArg(_, tr)))
                .toList.map{ case (tName, ext :: mix) => q"type $tName = $ext with ..$mix" }

              c.info(NoPosition, typeDefs.map(showCode(_)).mkString("\n"), true)
              name -> tr.prepend.body(typeDefs: _*)
            case system => system
          }

          trees.copy(agents = newAgs)
      }
    )
  )
}
