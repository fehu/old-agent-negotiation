package feh.tec.agents.light.spec

package object dsl {
  import scala.language.experimental.macros
  import feh.tec.agents.light.spec.{ NegotiationSpecification => NSpec }
  import feh.tec.agents.light.spec.macros.NegotiationSpecificationBuilder

  type Negotiation = dsl.NegotiationSpecification

  def spec(dsl: Negotiation): NSpec = macro NegotiationSpecificationBuilder.build
}
