package feh.tec.agents.lite.spec.macros.exp.impl.controller

import feh.tec.agents.lite.impl.NegotiationEnvironmentController
import feh.tec.agents.lite.spec.NegotiationSpecification.NegotiationDef
import feh.tec.agents.lite.spec.macros.exp.ControllerBuildingMacroExperimentalBase
import scala.reflect.macros.whitebox

/** Contains `MacroSegmentsTransform`s for stages before **EmbedAgentProps**
  */
trait BeforeAgentsProps[C <: whitebox.Context]{
  self: ControllerBuildingMacroExperimentalBase[C] =>

  def allBeforeAgentsProps(raw: NegotiationRaw) =
    ControllerSegmentParent ::
    ControllerSegmentEmbedIssuesAndDomainIteratorsCreators(raw) :: Nil

  def ControllerSegmentParent = MacroSegmentsTransform(
    _.prepend(ControllerBuildingStages.AggregateParents,
      MacroSegment{
        case Trees(controller, ags) =>
          val newController = controller
            .prepend.parents(c.typeOf[NegotiationEnvironmentController])
          Trees(newController, ags)
      }
    )
  )

  def ControllerSegmentEmbedIssuesAndDomainIteratorsCreators(raw: NegotiationRaw) = MacroSegmentsTransform(
    _.append(ControllerBuildingStages.EmbedIssues,
      MacroSegment{
        case trees@Trees(controller, _) =>
          import c.universe._

          val (issues, domainIteratorsCreators) = raw.vars.map{
            case Raw.VarDef(name, Raw.DomainDef(domain, tpe, domTpe)) =>
              val (domainMix, iteratorBuilder) = domTpe match {
                case t if t <:< typeOf[Range]     => tq"Domain.Range"       -> tq"DomainIteratorBuilder.Range"
                case t if t <:< tq"Set[$tpe]".tpe => tq"Domain.Small[$tpe]" -> tq"DomainIteratorBuilder.Generic"
              }
              val issue = q"$name -> new Var($name, _.isInstanceOf[$tpe]) with $domainMix { def domain: $domTpe = $domain }"
              val domainIteratorsCreator = q"$name -> (new $iteratorBuilder).asInstanceOf[DomainIteratorBuilder[Var#Domain, Var#Tpe]]"

              issue -> domainIteratorsCreator
          }.unzip
          val issuesByNeg = raw.negotiations.map{ case NegotiationDef(name, i) => q"$name -> Seq(..$i)" }

          trees.copy(controller = controller.append.body(q"""
            protected val issues: Map[String, Var] = Map(..$issues)
            protected val issuesByNegotiation: Map[String, Seq[String]] = Map(..$issuesByNeg)
            protected val domainIteratorsCreators: Map[String, DomainIteratorBuilder[Var#Domain, Var#Tpe]] = Map(..$domainIteratorsCreators)
          """
          ))
      }
    )
  )
}
