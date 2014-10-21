package feh.tec.agents.light.impl.agent

import akka.actor.{ActorSystem, Props}
import feh.tec.agents.light.impl.agent.create.AgentProps.{NegotiationInitExtended, NegotiationInit}
import feh.tec.agents.light.impl.spec.{PriorityAndProposalBasedAgentSpec, IteratingSpec}
import feh.tec.agents.light._
import feh.tec.agents.light.spec.MonoDefinition
import feh.tec.agents.light.spec.NegotiationSpecification.Interlocutors

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

  def PriorityAndProposalBasedIteratingAllVars(
                                    specification: PriorityAndProposalBasedAgentSpec[PPI.Ag, PPI.Lang] with IteratingSpec.AllVars[PPI.Ag, PPI.Lang]
                                                )(
                                    name: String, role: NegotiationRole, negotiationIds: Set[NegotiationId]
                                     ) =
    Props(new impl.agent.PriorityAndProposalBasedAgent[PPI.Lang](
      name, role, negotiationIds
    ) with impl.agent.DomainIteratingAllVars[PPI.Lang]
    {
      self: IteratingSpec.Agent[PPI.Lang] =>

      type Negotiation = Negotiation.HasProposal[PPI.Lang] with Negotiation.HasPriority with Negotiation.HasIterator with Negotiation.DynamicScope

      val spec = specification.asInstanceOf[PriorityAndProposalBasedAgentSpec[this.type, PPI.Lang] with IteratingSpec.AllVars[this.type, PPI.Lang]]

      def domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]] = ???

      protected def createNegotiation(id: NegotiationId): Negotiation = ???
    }
    )

  case class AgentProps[Arg](name: String, role: NegotiationRole, negotiationInits: Set[NegotiationInitExtended],
                             buildProps: (Agent.Id, Set[NegotiationInit], Arg) => Props)
  {
    lazy val id = Agent.Id(name, role)
    def props(arg: Arg) = buildProps(id, negotiationInits.map(_.init), arg)
    def create(arg: Arg)(implicit sys: ActorSystem) = AgentRef(id, sys.actorOf(props(arg), name))
  }

  object AgentProps{
    case class NegotiationInit(id: NegotiationId, issues: Set[Var])
    case class NegotiationInitExtended(init: NegotiationInit, scope: Interlocutors) {
      def id = init.id
      def issues = init.issues
    }
    trait CanProvide[Arg]{
      def get: Arg
    }
    case class AgentPropsBundle[Arg](props: AgentProps[Arg], provider: CanProvide[Arg]){
      def props()(implicit sys: ActorSystem): Props = props.props(provider.get)
      def create(implicit sys: ActorSystem) = props.create(provider.get)
    }
  }
}
