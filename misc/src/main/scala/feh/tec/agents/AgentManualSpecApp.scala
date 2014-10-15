package feh.tec.agents

import feh.tec.agents.light.{Var, NegotiationId}
import feh.tec.agents.light.impl.PriorityAndProposalBasedAgent

object AgentManualSpecApp {

  val spec = new PriorityAndProposalBasedAgent.Default{
    def nextValues: DefBADS[(NegotiationId) => Option[Map[Var, Any]]] = ???
  }
}
