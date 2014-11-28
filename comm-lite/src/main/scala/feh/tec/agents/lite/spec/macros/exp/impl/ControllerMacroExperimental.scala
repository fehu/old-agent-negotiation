package feh.tec.agents.lite.spec.macros.exp.impl

import akka.actor.Props
import feh.tec.agents.lite.spec
import feh.tec.agents.lite.spec.macros.exp.impl.agent.AgentsBuildingMacroExperimentalImpl
import feh.tec.agents.lite.spec.macros.exp.impl.controller.ControllerBuildingMacroExperimentalImpl
import feh.tec.agents.lite.spec.macros.{HasVarsSeparatingConstraintsBuilder, ActorBuildingMacroImpl, NegotiationBuildingMacroImpl}
import scala.reflect.macros.whitebox

class ControllerMacroExperimental[C <: whitebox.Context](val c: C)
  extends NegotiationBuildingMacroImpl[C] with ActorBuildingMacroImpl[C]
  with ControllerBuildingMacroExperimentalImpl[C] with AgentsBuildingMacroExperimentalImpl[C]
  with HasVarsSeparatingConstraintsBuilder[C]
{
  def cBuilder = new VarsSeparatingConstraintsBuilder

  def AllTransformation(raw: NegotiationRaw) = AgentSegmentsTransformation(raw) ::: ControllerSegmentsTransformation(raw)
  
  implicit lazy val StageOrdering = StagesOrdering(
    AgentStagesOrdering.precedence >> ControllerStagesOrdering.precedence
  )

  def anonControllerName = "$AnonNegotiationController"

  def controllerPropsExpr(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[Props] ={
    val negRaw = build(dsl)
    val segments = Function.chain(AllTransformation(negRaw))(MacroSegments.empty).segments
    val trees = Function.chain(segments)(Trees.empty(anonControllerName))
    val buildPropsExpr = actorCreatePropsExpr(trees.controller)
    c.universe.reify(buildPropsExpr.splice(Map()))
  }
}
