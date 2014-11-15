package feh.tec.agents.light.spec

import akka.actor.Props
import feh.tec.agents.light.spec.macros.{AgentSpecificationBuilder, NegotiationMacros}

package object dsl {
  import scala.language.experimental.macros

  def `def`(dsl: Agent): AgentSpecification = macro AgentSpecificationBuilder.build
  def controller(dsl: Negotiation): Props = macro NegotiationMacros.controller
}
