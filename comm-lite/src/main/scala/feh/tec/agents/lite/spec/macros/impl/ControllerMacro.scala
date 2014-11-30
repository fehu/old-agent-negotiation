package feh.tec.agents.lite.spec.macros.impl

import akka.actor.Props
import feh.tec.agents.lite.spec
import feh.tec.agents.lite.spec.macros.impl.agent.AgentsBuildingMacroImpl
import feh.tec.agents.lite.spec.macros.impl.controller.ControllerBuildingMacroImpl
import feh.tec.agents.lite.spec.macros.ActorBuildingMacroImpl
import scala.reflect.macros.whitebox

class ControllerMacro[C <: whitebox.Context](val c: C)
  extends NegotiationBuildingMacroImpl[C] with ActorBuildingMacroImpl[C]
  with ControllerBuildingMacroImpl[C] with AgentsBuildingMacroImpl[C]
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

object ControllerMacro {
  def controller(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[Props] = {
    val m = new ControllerMacro[c.type](c)
    m.controllerPropsExpr(dsl)
  }
}
