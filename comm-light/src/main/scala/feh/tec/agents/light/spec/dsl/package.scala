package feh.tec.agents.light.spec

import akka.actor.Props
import feh.tec.agents.light.EnvironmentController
import feh.tec.agents.light.spec.macros.{NegotiationControllerBuilder, AgentSpecificationBuilder, NegotiationSpecificationBuilder}
import feh.tec.agents.light.spec.{ NegotiationSpecification => NSpec }

package object dsl {
  import scala.language.experimental.macros

  def `def`(dsl: Negotiation): NSpec = macro NegotiationSpecificationBuilder.build
  def `def`(dsl: Agent): AgentSpecification = macro AgentSpecificationBuilder.build
  
  def controller(dsl: Negotiation): Props = macro NegotiationControllerBuilder.build
}
