package feh.tec.agents.light.spec

import akka.actor.Props
import feh.tec.agents.light._
import feh.tec.agents.light.impl.PriorityAndProposalBasedAgent

trait AgentSpecification {
  type Agent
  type BuildArgs

  def build(args: BuildArgs): Props
}

object AgentSpecification{

  trait PriorityAndProposalBased extends AgentSpecification{

    type Lang <: Language.ProposalBased with Language.HasPriority
    
    def nothingToPropose: PriorityAndProposalBasedAgent.BaseDef[NegotiationId => Unit]

    protected def beforeEachMessage: PriorityAndProposalBasedAgent.MonoDef[Lang#Msg => Unit]

    def setNextProposal: PriorityAndProposalBasedAgent.BaseDef[NegotiationId => Unit]

    def nextValues: PriorityAndProposalBasedAgent.BaseDef[NegotiationId => Option[Map[Var, Any]]]

    def onProposal: PriorityAndProposalBasedAgent.MonoDef[PartialFunction[Lang#Proposal, Any]]

    def onRejection: PriorityAndProposalBasedAgent.MonoDef[PartialFunction[Lang#Rejection, Any]]

    def onAcceptance: PriorityAndProposalBasedAgent.MonoDef[PartialFunction[Lang#Acceptance, Any]]

    def updateCurrentProposal: PriorityAndProposalBasedAgent.BaseDef[NegotiationId => Unit]

//    def requestPriorityRaise: PriorityAndProposalBasedAgent.ExtDef[NegotiationId => Lang#PriorityRaiseRequest]

//    def comparePriority: PriorityAndProposalBasedAgent.ExtDef[(Lang#Msg, (Priority, Priority) => Boolean) => Boolean]

    def priorityNegotiationHandler: PriorityAndProposalBasedAgent.MonoDef[PriorityNegotiationHandler[Lang] ]

    def stop: PriorityAndProposalBasedAgent.BaseDef[Unit]

    def reset: PriorityAndProposalBasedAgent.BaseDef[Unit]

    def start: PriorityAndProposalBasedAgent.BaseDef[Unit]
  }

}