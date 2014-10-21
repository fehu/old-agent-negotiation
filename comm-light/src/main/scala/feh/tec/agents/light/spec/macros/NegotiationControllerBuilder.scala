package feh.tec.agents.light.spec.macros

import feh.tec.agents.light.{EnvironmentController, spec}
import scala.reflect.macros.whitebox

object NegotiationControllerBuilder {
  def build(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[EnvironmentController] = {
    import c.universe._

    val ExtractRaw = NegotiationSpecificationBuilder.Raw.Extract[c.type](c)
    val ExtractRaw(variables, negotiations, agents, spawns, timings, timeouts) = NegotiationSpecificationBuilder.raw[c.type](c)(dsl)

    c.Expr(q"""
      import feh.tec.agents.light.impl.NegotiationEnvironmentController
      import akka.util.Timeout

      new NegotiationEnvironmentController{
        protected lazy val initialAgents = ???
        protected lazy val systemAgentsProps = ???
        protected lazy val timeouts = new NegotiationEnvironmentController.Timeouts {
          lazy val initialize: Timeout = ???
          lazy val start: Timeout = ???
          lazy val stop: Timeout = ???
          lazy val reset: Timeout = ???
        }
      }
     """)
  }
}

object AgentPropsBundle{

}