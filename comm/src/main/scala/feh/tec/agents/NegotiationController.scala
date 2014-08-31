package feh.tec.agents

import feh.tec.agents.SystemMessage.ScopeUpdate
import feh.tec.agents.impl.SystemAgent

trait NegotiationController extends SystemAgent{

  def agents: Seq[AgentRef]
  def systemAgents: Seq[AgentRef]

  def start()
  def stop()
}

object NegotiationController{

  trait ScopesInitialization{
    self: NegotiationController =>

    def scopeFor(ag: AgentRef, in: NegotiationId): Set[AgentRef]

    def updateScopes(neg: NegotiationId) = agents foreach {
      ag => ag ! ScopeUpdate.NewScope(scopeFor(ag, neg), neg)
    }
  }

}
