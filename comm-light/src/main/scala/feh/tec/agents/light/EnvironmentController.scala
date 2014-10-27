package feh.tec.agents.light

import feh.tec.agents.light.spec.NegotiationSpecification.Interlocutors

/** controls the negotiation environment: agents creation/termination */
trait EnvironmentController extends SystemAgent{

  /** prepare negotiations environment */
  def initialize()
  /** start negotiations */
  def start()
  /** stop(pause) negotiations */
  def stop()
  /** reset negotiations (after stop) */
  def reset()
}

/** scope updates */
trait DynamicEnvironmentController{
  controller: EnvironmentController =>
  
  def initialAgentsCreated(ag: Map[AgentRef, Map[NegotiationId, Interlocutors]])
  def agentAdded(ag: AgentRef, neg: Set[NegotiationId])
  def agentRemoved(ag: AgentRef, neg: Set[NegotiationId])
}
