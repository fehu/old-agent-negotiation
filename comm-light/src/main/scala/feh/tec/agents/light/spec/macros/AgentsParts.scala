package feh.tec.agents.light.spec.macros

import feh.tec.agents.light.Language.{HasPriority, ProposalBased}
import feh.tec.agents.light.spec.AgentSpecification.{Iterating, PriorityAndProposalBased}
import feh.tec.agents.light.{DomainIteratorBuilder, Var, NegotiationId, Language}
import feh.tec.agents.light.impl.agent.{DomainIteratingAllVars, interfaces}
import feh.tec.agents.light.impl.agent

import scala.reflect.macros.whitebox

object AgentsParts {
  object PriorityAndProposalBased extends AgentDefinitionPart(canBeMixed = false, interfaces.priorityAndProposalBased){
    def tpe[C <: whitebox.Context](c: C): c.Type = c.typeOf[agent.PriorityAndProposalBasedAgent[Language.ProposalBased with Language.HasPriority]]

    def langTpe[C <: whitebox.Context](c: C) = {
      import c.universe._
      tq"Language.ProposalBased with Language.HasPriority"
    }

    def parentTree[C <: whitebox.Context](c: C): c.Tree = {
      import c.universe._
      tq"feh.tec.agents.light.impl.agent.PriorityAndProposalBasedAgent[${langTpe[c.type](c)}]"
    }

  }
  object IteratingAllVars extends AgentDefinitionPart(canBeMixed = true, interfaces.iteratingAll){
    def tpe[C <: whitebox.Context](c: C): c.Type = c.typeOf[DomainIteratingAllVars[Language.ProposalBased with Language.HasPriority]]

    def langTpe[C <: whitebox.Context](c: C) = {
      import c.universe._
      tq"Language.ProposalBased with Language.HasPriority"
    }

    def parentTree[C <: whitebox.Context](c: C): c.type#Tree = {
      import c.universe._
      tq"feh.tec.agents.light.impl.agent.DomainIteratingAllVars[${langTpe[c.type](c)}]"
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
