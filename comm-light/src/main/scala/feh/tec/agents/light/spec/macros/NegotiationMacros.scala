package feh.tec.agents.light.spec.macros

import akka.actor.Props
import feh.tec.agents.light.spec

import scala.reflect.macros.whitebox

object NegotiationMacros {
  def controller(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[Props] = {
    val m = new ControllerMacro[c.type](c)

    val x = m.controllerPropsExpr(dsl, m.Trees.empty("$ControllerAnonClass"), new m.VarsSeparatingConstraintsBuilder) //.asInstanceOf[c.Expr[Props]]
    c.info(c.universe.NoPosition, "controllerPropsExpr = " + c.universe.showCode(x.tree), true)
    x
  }

  class ControllerMacro[C <: whitebox.Context](val c: C)
    extends NegotiationBuildingMacroImpl[C] with ControllerBuildingMacroImpl[C] with ControllerBuildingAgentsMacroImpl[C]
    with ActorBuildingMacroImpl[C] with HasVarsSeparatingConstraintsBuilder[C]
}
