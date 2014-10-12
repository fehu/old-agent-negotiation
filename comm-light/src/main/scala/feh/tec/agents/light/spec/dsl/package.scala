package feh.tec.agents.light.spec

import feh.tec.agents.light.spec.macros.AgentSpecificationBuilder
import feh.tec.agents.light.spec.{ NegotiationSpecification => NSpec }
import feh.tec.agents.light.spec.macros.NegotiationSpecificationBuilder

package object dsl {
  import scala.language.experimental.macros

  def `def`(dsl: Negotiation): NSpec = macro NegotiationSpecificationBuilder.build
  def `def`(dsl: Agent): AgentSpecification = macro AgentSpecificationBuilder.build
}
