package feh.tec.agents.lite.spec.macros.exp.impl

import feh.tec.agents.lite.SystemAgent
import feh.tec.agents.lite.spec.macros.exp.{ControllerBuildingMacroExperimental, AgentsBuildingMacroExperimentalBase}
import scala.reflect.macros.whitebox
import feh.util._

/**
 * Contains `MacroSegmentsTransform`s for `TypesDefinitions` stage
 */
trait TypesDefinitions[C <: whitebox.Context]{
  self: AgentsBuildingMacroExperimentalBase[C] with ControllerBuildingMacroExperimental[C] =>

  def allTypesDefinitions = AgentSegmentTypesDefinitions :: Nil

  def ExtractLangs = ???

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
