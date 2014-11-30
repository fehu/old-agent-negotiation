package feh.tec.agents.lite.spec

import akka.actor.Props
import feh.tec.agents.lite.spec.macros.experimental.AgentSpecificationBuilder
import feh.tec.agents.lite.spec.macros.impl.ControllerMacro

package object dsl {
  import scala.language.experimental.macros

  def `def`(dsl: Agent): AgentSpecification = macro AgentSpecificationBuilder.build
  def controller(dsl: Negotiation): Props = macro ControllerMacro.controller
}
