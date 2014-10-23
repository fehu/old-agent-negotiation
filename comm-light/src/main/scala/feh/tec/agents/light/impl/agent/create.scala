package feh.tec.agents.light.impl.agent

import akka.actor.{ActorSystem, Props}
import feh.tec.agents.light.impl.spec.{PriorityAndProposalBasedAgentSpec, IteratingSpec}
import feh.tec.agents.light._
import feh.tec.agents.light.spec.AgentProps.NegotiationInit
import feh.tec.agents.light.spec.{AgentProps, MonoDefinition}

object create {
  trait SpecExt[Ow]{
    // copied from the one from Agent DSL
    implicit class ExtendableMonoDefinitionWrapper[Def](eDef: MonoDefinition[Ow, Def]){
      def <:=(d: Ow => Def) = eDef.DefExtension = eDef.DefExtension.copy(`override` = Some(d))
      def :=(d: Def) = eDef.DefExtension = eDef.DefExtension.copy(`override` = Some((ow: Ow) => d))
    }
  }


  object PPI{
    type Lang = NegotiationLanguage with Language.ProposalBased with Language.HasPriority
    type Ag = impl.spec.IteratingSpec.Agent[Lang]

    trait AllVarsSpec extends PriorityAndProposalBasedAgentSpec[Ag, Lang] with IteratingSpec.AllVars[Ag, Lang] with SpecExt[Ag]
  }

  trait PriorityAndProposalBasedIteratingAllVars
    extends impl.agent.PriorityAndProposalBasedAgent[PPI.Lang] with impl.agent.DomainIteratingAllVars[PPI.Lang]
  {
    def domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]] = ???

    protected def createNegotiation(id: NegotiationId): Negotiation = ???
  }

  def PriorityAndProposalBasedIteratingAllVars(
                                    specification: PriorityAndProposalBasedAgentSpec[PPI.Ag, PPI.Lang] with IteratingSpec.AllVars[PPI.Ag, PPI.Lang]
                                                ): AgentProps.Building[None.type] =
    AgentProps.Building(AgentProps.NoArgs, (name: String, role: NegotiationRole, negInit: Set[NegotiationInit], arg: None.type) =>
      Props(
        new impl.agent.PriorityAndProposalBasedAgent[PPI.Lang](name, role, negInit) with PriorityAndProposalBasedIteratingAllVars
        {
          self: IteratingSpec.Agent[PPI.Lang] =>

          type Negotiation = Negotiation.HasProposal[PPI.Lang] with Negotiation.HasPriority with Negotiation.HasIterator with Negotiation.DynamicScope

          val spec = specification.asInstanceOf[PriorityAndProposalBasedAgentSpec[this.type, PPI.Lang] with IteratingSpec.AllVars[this.type, PPI.Lang]]
        }
      )
    )
}
