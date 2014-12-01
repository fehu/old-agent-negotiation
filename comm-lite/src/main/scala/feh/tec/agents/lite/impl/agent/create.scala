package feh.tec.agents.lite.impl.agent

import akka.actor.{ActorSystem, Props}
import feh.tec.agents.lite.impl.agent.create.PPI.Lang
import feh.tec.agents.lite.impl.spec.{PriorityAndProposalBasedAgentSpec, IteratingSpec}
import feh.tec.agents.lite._
import feh.tec.agents.lite.spec.AgentSpecification.{Iterating, PriorityAndProposalBased}
import feh.tec.agents.lite.spec.{AgentSpecification, ExtendableDefinition, MonoDefinition}

object create {
  trait SpecExt[Ow]{
    // copied from the one from Agent DSL
    implicit class ExtendableMonoDefinitionWrapper[Def](eDef: MonoDefinition[Ow, Def]){
      /** override main ext point */
      def <:=(d: Ow => Def) = eDef.DefExtension = eDef.DefExtension.copy(`override` = Some(d))
      /** override main ext point */
      def :=(d: Def) = eDef.DefExtension = eDef.DefExtension.copy(`override` = Some((ow: Ow) => d))
      /** override main ext point */
      def andThen(d: Ow => ((Ow => Def) => Def)) = eDef.DefExtension = eDef.DefExtension.`override` match {
        case Some(overr) => eDef.DefExtension.copy(`override` = Some((ow: Ow) => d(ow)(overr)))
        case None => eDef.DefExtension.copy(`override` = Some((ow: Ow) => d(ow)(eDef.DefExtension.default)))
      }
    }

    implicit class ExtendableBeforeAndAfterDefinitionWrapper[Def](eDef: ExtendableDefinition[Ow, Def] with ExtendableDefinition.BeforeAndAfter[Ow, Def]){
      /** override after ext point */
      def after(d: Ow => (Def => Def)) = eDef.AfterExtension = eDef.AfterExtension.copy(`override` = Some(d))
      /** override before ext point */
      def before(d: Ow => Unit) = eDef.BeforeExtension = eDef.BeforeExtension.copy(`override` = Some(d))
    }
  }


  object PPI{
    type Lang = NegotiationLanguage with Language.ProposalBased with Language.HasPriority
    type Ag[L <: Lang] = impl.spec.IteratingSpec.Agent[L]

    trait AllVarsSpec[A <: Ag[L], L <: Lang] extends PriorityAndProposalBasedAgentSpec[A, L] with IteratingSpec.AllVars[A, L] with SpecExt[A]
    trait DynIssuesSpec[A <: Ag[L], L <: Lang] extends PriorityAndProposalBasedAgentSpec[A, L] with IteratingSpec.ChangingIssues[A, L] with SpecExt[A]
  }

/*
  trait PriorityAndProposalBasedIteratingAllVars
    extends impl.agent.PriorityAndProposalBasedAgent[PPI.Lang] with impl.agent.DomainIteratingAllVars[PPI.Lang]
  {
    def domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]] = ???

    protected def createNegotiation(id: NegotiationId): Negotiation = ???
  }
*/

//  def PriorityAndProposalBasedIteratingAllVars(
//                                    specification: PriorityAndProposalBasedAgentSpec[PPI.Ag, PPI.Lang] with IteratingSpec.AllVars[PPI.Ag, PPI.Lang]
//                                                ): AgentProps.Building[None.type] =
//    AgentProps.Building(AgentProps.NoArgs, (name: String, role: NegotiationRole, negInit: Set[NegotiationInit], arg: None.type) =>
//      Props(
//        new impl.agent.PriorityAndProposalBasedAgent[PPI.Lang](name, role, negInit) with PriorityAndProposalBasedIteratingAllVars
//        {
//          self: IteratingSpec.Agent[PPI.Lang] =>
//
//          type Negotiation = Negotiation.HasProposal[PPI.Lang] with Negotiation.HasPriority with Negotiation.HasIterator with Negotiation.DynamicScope
//
//          val spec = specification.asInstanceOf[PriorityAndProposalBasedAgentSpec[this.type, PPI.Lang] with IteratingSpec.AllVars[this.type, PPI.Lang]]
//        }
//      )
//    )
}
