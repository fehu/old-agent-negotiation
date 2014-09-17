package feh.tec.agents.spec

package object dsl {
  import scala.language.experimental.macros
  import feh.tec.agents.spec.{ NegotiationSpecification => NSpec }
  import feh.tec.agents.spec.macros.NegotiationSpecificationBuilder

  type Negotiation = dsl.NegotiationSpecification

  def spec(dsl: Negotiation): NSpec = macro NegotiationSpecificationBuilder.build
}
