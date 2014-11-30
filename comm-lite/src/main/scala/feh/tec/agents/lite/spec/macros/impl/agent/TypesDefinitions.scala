package feh.tec.agents.lite.spec.macros.impl.agent

import feh.tec.agents.lite.spec.macros.AgentsBuildingMacroBase
import feh.tec.agents.lite.{Language, SystemAgent}
import feh.util._
import scala.reflect.macros.whitebox

trait TypesDefinitionsHelpers[C <: whitebox.Context]{
  self: AgentsBuildingMacroBase[C] =>

  import c.universe._
  
  def langTypeName(anonAgentClassName: String) = TypeName(anonAgentClassName + "Lang")

  protected def decomposeTypesAndBounds(from: c.Type) = from match {
    case TypeBounds(_, RefinedType(tpes, _)) => tpes
    case TypeBounds(_, ref: TypeRef) => ref :: Nil
    case RefinedType(tpes, _) => tpes
    case ref: TypeRef => ref :: Nil
  }

  protected def unitedType(of: List[c.Type]) = internal.refinedType(of, internal.newScopeWith())

  protected def replaceTypeArg(in: c.Type, ofType: c.Type, replacement: c.Type) = in map{
    case TypeRef(pre, sym, args) =>
      val newArgs = args.map{
        case tpe if tpe <:< ofType => replacement
        case other => other
      }
      internal.typeRef(pre, sym, newArgs)
    case other => other
  }

  protected def replaceLanguageTypeArg(in: c.Type, lang: c.Type) = replaceTypeArg(in, typeOf[Language], lang)

  protected def langType(of: ActorTrees): Option[c.Type] = of.body collectFirst{
    case q"type $name = $tpe" if name == langTypeName(of.className) => tpe.tpe
  }

  protected def replaceLang(tr: ActorTrees) =
    langType(tr).map(l => replaceLanguageTypeArg(_: c.Type, l)).getOrElse(identity[c.Type] _)
}

/**
 * Contains `MacroSegmentsTransform`s for `TypesDefinitions` stage
 */
trait TypesDefinitions[C <: whitebox.Context] extends TypesDefinitionsHelpers[C]{
  self: AgentsBuildingMacroBase[C] =>

  import c.universe._

  def allTypesDefinitions =
    AgentSegmentLangDefinition ::
    AgentSegmentTypesDefinitions ::
    AgentSegmentReplaceLang :: Nil

  /** extract agent's `Lang` type and defines it in the body as [[langTypeName]]
   */
  def AgentSegmentLangDefinition = MacroSegmentsTransform{
    _.append(AgentBuildingStages.TypesDefinitions,
      MacroSegment{
        case trees =>
          val newAgents = trees.agents.map{
            case (name, tr@ActorTrees(cName, parents, _, _)) =>
              val langCompTypes = parents.flatMap(_.typeArgs.filter(_.resultType <:< typeOf[Language])) flatMap decomposeTypesAndBounds
              val langType = unitedType(langCompTypes.distinct)

              name -> tr.prepend.body(q"type ${langTypeName(cName)} = $langType")
          }

          trees.copy(agents = newAgents)
      }
    )
  }

  /** defines abstract types
   */
  def AgentSegmentTypesDefinitions = MacroSegmentsTransform(
    _.append(AgentBuildingStages.TypesDefinitions,
      MacroSegment{
        case trees =>
          import c.universe._

          val newAgs = trees.agents.map{
            case (name, tr@ActorTrees(_, parents, _, _)) if !parents.exists(_ <:< typeOf[SystemAgent]) =>
              val abstractTypeMembers = parents
                .flatMap(
                  _.members
                    .withFilter(d => d.name.isTypeName && d.isAbstract && !d.isClass)
                    .map(
                      sym =>
                        sym.asType.name ->
                          (sym.typeSignature match {
                            case TypeBounds(_, RefinedType(tpes, _)) => tpes
                            case TypeBounds(_, ref: TypeRef) => ref :: Nil
                            case other => c.abort(NoPosition, other.toString)
                          })
                    )
                ).groupBy(_._1)
                .mapValues(_.unzip._2.flatten.distinctBy(_.typeSymbol.name))
                .filter(_._2.nonEmpty)

              val typeDefs = abstractTypeMembers
                .mapValues(_.map(replaceLang(tr)))
                .toList.map{ case (tName, ext :: mix) => q"type $tName = $ext with ..$mix" }

              c.info(NoPosition, typeDefs.map(showCode(_)).mkString("\n"), true)
              name -> tr.prepend.body(typeDefs: _*)
            case system => system
          }

          trees.copy(agents = newAgs)
      }
    )
  )

  /** replaces `Lang` type parameter in agents' parents
    */
  def AgentSegmentReplaceLang = MacroSegmentsTransform(
    _.append(AgentBuildingStages.TypesDefinitions,
    {
      MacroSegment{
        case trees =>
          val newAgs = trees.agents.map{
            case (name, tr@ActorTrees(cName, parents, _, _)) =>
              def replLang = replaceLang(tr)
              val newParents = parents.map(replLang)
              name -> tr.copy(parents = newParents)
          }
          trees.copy(agents = newAgs)
      }
    }
    )
  )
}
