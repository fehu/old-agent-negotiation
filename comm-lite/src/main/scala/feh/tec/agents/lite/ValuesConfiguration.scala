package feh.tec.agents.lite

/** The values set by some agents for a negotiation and their priorities */
class PartialValuesConfiguration(val configurations: Map[Priority, Map[Var, Any]], val negotiation: NegotiationId){
  def filter(f: (Priority, Map[Var, Any]) => Boolean): PartialValuesConfiguration = new PartialValuesConfiguration(
    configurations.filter(f.tupled), negotiation
  )

  def size = configurations.size

  override def toString = s"PartialValuesConfiguration($configurations in $negotiation)"
}

/** The full set of the values for a negotiation */
class ValuesConfiguration(configurations: Map[Priority, Map[Var, Any]], negotiation: NegotiationId)
  extends PartialValuesConfiguration(configurations, negotiation)

case class PartialSolution(issues: Set[Var], values: Map[Priority, Map[Var, Any]])

//case class Solution(values: Map[Priority, Map[Var, Any]])