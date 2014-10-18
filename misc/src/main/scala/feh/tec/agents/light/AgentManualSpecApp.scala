package feh.tec.agents.light

import akka.actor.Props
import feh.tec.agents.light.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}

object AgentManualSpecApp {
  type Lang = NegotiationLanguage with Language.HasPriority with Language.ProposalBased

  def agentSpec[Ag <: impl.spec.IteratingSpec.Agent[Lang]](ag: Ag) =
    new PriorityAndProposalBasedAgentSpec[Ag, Lang] with IteratingSpec.AllVars[Ag, Lang]{

    }

  val ag = Props(new impl.agent.PriorityAndProposalBasedAgent[Lang](
    "test", new Role("test") with NegotiationRole, Set()
    ) with impl.agent.DomainIteratingAllVars[Lang]
  {
    self: IteratingSpec.Agent[Lang] =>

    type Negotiation = Negotiation.HasProposal[Lang] with Negotiation.HasPriority with Negotiation.HasIterator with Negotiation.DynamicScope

    val spec = AgentManualSpecApp.agentSpec[this.type](this)

    def domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]] = ???

    protected def createNegotiation(id: NegotiationId): Negotiation = ???
  }
  )
}
