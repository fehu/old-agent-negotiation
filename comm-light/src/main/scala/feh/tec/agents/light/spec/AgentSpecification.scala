package feh.tec.agents.light.spec

import akka.actor.Props
import feh.tec.agents.light.Message.{PriorityRaiseResponse, PriorityRaiseRequest}
import feh.tec.agents.light._
import feh.tec.agents.light
import feh.tec.agents.light.impl.PriorityAndProposalBasedAgent

trait AgentSpecification {
//  type Agent <: AbstractAgent
//  type BuildArgs

//  def build(args: BuildArgs): Props

//  /** for referring self */
//  protected val agent: Agent
}

trait AgentSpecificationExt[Ag <: PriorityAndProposalBasedAgent[_]]{

  trait DefExt[T] extends ExtendableDefinition[Ag, T]
  /** Directly Settable */
  class DefDS[T](default: Ag => T) extends MonoDefinition[Ag, T](default) with DefExt[T]
  /** Directly Settable Hidden */
  class DefDSH[T](default: Ag => T) extends HiddenMonoDefinition[Ag, T](default) with DefExt[T]
  /** Has Before and After extensions, Directly Settable */
  class DefBADS[T](default: Ag => T) extends DefDS[T](default) with ExtendableDefinition.BeforeAndAfter[Ag, T]{
    override def extensionPoints = super[DefDS].extensionPoints ++ super[BeforeAndAfter].extensionPoints
    override def get(implicit owner: Ag): T = {
      BeforeExtension.get.apply(owner)
      AfterExtension.get(owner) apply DefExtension.get(owner)
    }
  }
}

object AgentSpecification{

  trait PriorityAndProposalBased[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]
    extends AgentSpecification with AgentSpecificationExt[Ag]
  {
//    type Agent = Ag

    def initialize: DefBADS[Unit]
    def start: DefBADS[Unit]
    def stop: DefBADS[Unit]
    def reset: DefBADS[Unit]

    def beforeEachMessage: DefDS[Lang#Msg => Unit]

    def onProposal: DefDS[PartialFunction[Lang#Proposal, Any]]
    def onAcceptance: DefDS[PartialFunction[Lang#Acceptance, Any]]
    def onRejection: DefDS[PartialFunction[Lang#Rejection, Any]]

    def moreProcess: DefDS[PartialFunction[Lang#Msg, Any]]

    def nextValues: DefBADS[NegotiationId => Option[Map[Var, Any]]]

    def setNextProposal: DefBADS[NegotiationId => Lang#Proposal]

    def updateCurrentProposal: DefBADS[NegotiationId => Unit]

    def nothingToPropose: DefBADS[NegotiationId => Unit]

//    def requestPriorityRaise: ExtDef[NegotiationId => Lang#PriorityRaiseRequest]

//    def comparePriority: ExtDef[(Lang#Msg, (Priority, Priority) => Boolean) => Boolean]
  }

  trait NegotiatesPriority[Ag <: PriorityAndProposalBasedAgent[Lang] with light.NegotiatesPriority[Lang], Lang <: Language.ProposalBased with Language.HasPriorityNegotiation]
    extends AgentSpecificationExt[Ag]
  {
    def priorityNegotiationHandler[R](f: AgentSpecification.PriorityNegotiationHandler[Ag, Lang] => Unit)
    protected[light] def priorityNegotiationHandler: DefExt[AgentSpecification.PriorityNegotiationHandler[Ag, Lang]]
  }

  trait PriorityNegotiationHandler[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.HasPriorityNegotiation with Language.ProposalBased]
    extends AgentSpecificationExt[Ag]
  {
    def onPriorityUpdate: DefBADS[(NegotiationId, Map[AgentRef, Lang#PriorityRaiseResponse]) => Unit]

    def decide: DefDS[NegotiationId => Map[AgentRef, Lang#PriorityRaiseRequest] => Lang#PriorityRaiseResponse]

    def start: DefDS[NegotiationId => Lang#PriorityRaiseRequest]

    def process: DefDS[PartialFunction[Lang#Priority, Any]]

    def evidence: DefDS[NegotiationId => Any]
  }

  trait Iterating[Ag <: PriorityAndProposalBasedAgent[Lang] with ProposalEngine.Iterating[Lang], Lang <: Language.HasPriority with Language.ProposalBased]
    extends AgentSpecificationExt[Ag]
  {
    def newIterator: DefBADS[NegotiationId => ProposalEngine.DomainIterator]
  }
}

// is a marker
@deprecated("is related to a negotiation, not the entire agent")
trait RequiresDistinctPriority{
  self: AgentSpecification =>

}
