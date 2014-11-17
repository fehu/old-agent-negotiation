package feh.tec.agents.light.spec.macros

import akka.actor.Props
import feh.tec.agents.light.spec

import scala.reflect.macros.whitebox

object NegotiationMacros {
  def controller(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[Props] = {
    val m = new ControllerMacro[c.type](c)
    m.controllerPropsExpr(dsl, m.Trees.empty("$ControllerAnonClass"), new m.VarsSeparatingConstraintsBuilder)
  }

  class ControllerMacro[C <: whitebox.Context](val c: C)
    extends NegotiationBuildingMacroImpl[C] with ControllerBuildingMacroImpl[C] with AgentsBuildingMacroImpl[C]
    with ActorBuildingMacroImpl[C] with HasVarsSeparatingConstraintsBuilder[C]
}
