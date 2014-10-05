package feh.tec.agents.light.impl

import feh.tec.agents.light._

class PriorityAndProposalBasedAgent[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityProposalBasedAgent[Lang] with DynamicScopeSupport[Lang] with SpeakingSystemSupport[Lang]
{
  def nothingToPropose(neg: NegotiationId) = ???

  type Negotiation = this.type

  def setNextProposal(neg: NegotiationId) = ???

  def nextValues(neg: NegotiationId) = ???

  def onProposal = ???

  def onRejection = ???

  def onAcceptance = ???

  def updateCurrentProposal(neg: NegotiationId) = ???

  def requestPriorityRaise(neg: NegotiationId) = ???

  def comparePriority(msg: Lang#Msg, f: (Priority, Priority) => Boolean) = ???

  def priorityNegotiationHandler = ???

  def negotiations = ???

  override val role: NegotiationRole = _

  def process = ???

  val ref: AgentRef = _

  def stop() = ???

  def reset() = ???

  def start() = ???

  val name: String = _

  protected def beforeEachMessage(msg: Lang#Msg) = ???
}
