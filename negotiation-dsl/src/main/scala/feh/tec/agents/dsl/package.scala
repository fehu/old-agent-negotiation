package feh.tec.agents

import feh.tec.agents

package object dsl {
  import scala.language.experimental.macros

  type Negotiation = agents.impl.NegotiationSpecificationDSL

  def spec(dsl: Negotiation): impl.NegotiationSpecification = macro agents.macros.NegotiationSpecificationBuilder.build
}
