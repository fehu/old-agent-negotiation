package feh.tec.agents

/** define the negotiation environment */
trait NegotiationSpecification{
  type VarDef
  type AgentDef
  type NegotiationDef
  type Config

  def variables: Seq[VarDef]
  def negotiations: Seq[NegotiationDef]
  def agents: Seq[AgentDef]
  def config: Config
}
