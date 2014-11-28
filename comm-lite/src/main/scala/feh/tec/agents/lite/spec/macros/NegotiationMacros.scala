package feh.tec.agents.lite.spec.macros

import akka.actor.Props
import feh.tec.agents.lite.spec
import feh.tec.agents.lite.spec.macros.exp.impl.ControllerMacroExperimental

import scala.reflect.macros.whitebox

object NegotiationMacros {
  def controller(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[Props] = {
    val m = new ControllerMacro[c.type](c)
    m.controllerPropsExpr(dsl, m.Trees.empty("$ControllerAnonClass"), new m.VarsSeparatingConstraintsBuilder)
  }

  def controllerExperimental(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[Props] = {
    val m = new ControllerMacroExperimental[c.type](c)
    m.controllerPropsExpr(dsl)
  }

  class ControllerMacro[C <: whitebox.Context](val c: C)
    extends NegotiationBuildingMacroImpl[C] with ControllerBuildingMacroImpl[C] with AgentsBuildingMacroImpl[C]
    with ActorBuildingMacroImpl[C] with HasVarsSeparatingConstraintsBuilder[C]
}
