package feh.tec.agents.light.spec.macros

import feh.tec.agents.light.Language.{HasPriority, ProposalBased}
import feh.tec.agents.light.spec.AgentSpecification.{Iterating, PriorityAndProposalBased}
import feh.tec.agents.light.{DomainIteratorBuilder, Var, NegotiationId, Language}
import feh.tec.agents.light.impl.agent.interfaces

import scala.reflect.macros.whitebox

object AgentsParts {
  object PriorityAndProposalBased extends AgentDefinitionPart(canBeMixed = false, interfaces.priorityAndProposalBased){
//    def parent = None
    def parentTree[C <: whitebox.Context](c: C): c.Tree = {
      import c.universe._
      tq"feh.tec.agents.light.impl.agent.PriorityAndProposalBasedAgent[Language.ProposalBased with Language.HasPriority]"
    }

  }
  object IteratingAllVars extends AgentDefinitionPart(canBeMixed = true, interfaces.iteratingAll){
//    def parent = Some(PriorityAndProposalBased)
    def parentTree[C <: whitebox.Context](c: C): c.type#Tree = {
      import c.universe._
      tq"feh.tec.agents.light.impl.agent.DomainIteratingAllVars[Language.ProposalBased with Language.HasPriority]"
    }
  }

/*
  new feh.tec.agents.light.impl.agent.PriorityAndProposalBasedAgent[Language.ProposalBased with Language.HasPriority](???, ???, ???, ???)
    with feh.tec.agents.light.impl.agent.DomainIteratingAllVars[Language.ProposalBased with Language.HasPriority]
  {
    type Negotiation = this.type
    override val spec: PriorityAndProposalBased[this.type, ProposalBased with HasPriority] with Iterating[this.type, ProposalBased with HasPriority] = _

    def domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]] = ???

    protected def createNegotiation(id: NegotiationId): Negotiation = ???
  }
*/
}
