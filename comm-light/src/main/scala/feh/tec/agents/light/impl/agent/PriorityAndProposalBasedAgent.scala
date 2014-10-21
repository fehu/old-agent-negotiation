package feh.tec.agents.light.impl.agent

import feh.tec.agents.light.spec.AgentProps.NegotiationInit
import feh.tec.agents.light.spec.AgentSpecification
import feh.tec.agents.light._
import feh.util._
import feh.tec.agents.light.impl

abstract class PriorityAndProposalBasedAgent[Lang <: Language.ProposalBased with Language.HasPriority](
            val name: String,
            override val role: NegotiationRole,
            negotiationInit: Set[NegotiationInit]
          )
  extends impl.PriorityAndProposalBasedAgent[Lang]
{
  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang]

  val spec: AgentSpecification.PriorityAndProposalBased[this.type, Lang]

  implicit def owner: this.type = this

  /* should be defined by ExtendableDefinition */

  def nothingToPropose(neg: NegotiationId): Unit            = spec.nothingToPropose.get apply neg
  protected def beforeEachMessage(msg: Lang#Msg): Unit      = spec.beforeEachMessage.get apply msg
  def setNextProposal(neg: NegotiationId): Unit             = spec.setNextProposal.get apply neg
  def nextValues(neg: NegotiationId): Option[Map[Var, Any]] = spec.nextValues.get apply neg
  def onProposal: PartialFunction[Lang#Proposal, Any]       = spec.onProposal.get
  def onRejection: PartialFunction[Lang#Rejection, Any]     = spec.onRejection.get
  def onAcceptance: PartialFunction[Lang#Acceptance, Any]   = spec.onAcceptance.get
  def updateCurrentProposal(neg: NegotiationId): Unit       = spec.updateCurrentProposal.get apply neg
  def stop(): Unit                                          = spec.stop.get
  def reset(): Unit                                         = spec.reset.get
  def start(): Unit                                         = spec.start.get
  def initialize(): Unit                                    = spec.initialize.get

  val negotiationIds: Set[NegotiationId] = negotiationInit.map(_.id)

  lazy val priorityNegotiationHandler: PriorityNegotiationHandler[Lang] = new PriorityNegotiationHandlerImpl(
    spec.priorityNegotiationHandler.get, owner, this.get, this.sendToAll
  )
}

class PriorityNegotiationHandlerImpl[Owner <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority](
                val spec: AgentSpecification.PriorityNegotiationHandler[Owner , Lang],
                implicit val owner: Owner,
                protected val get: NegotiationId => AbstractNegotiation,
                protected val sendAll: Lang#Msg => Unit
              )
  extends impl.PriorityNegotiationHandlerImpl[Lang]
{
  def onPriorityUpdate(f: (NegotiationId, Option[Priority])): Any                            = spec.onPriorityUpdate.get.tupled(f)
  def decide(requests: Map[AgentRef, Lang#PriorityRaiseRequest]): Lang#PriorityRaiseResponse = spec.decide.get apply requests
  def start(neg: NegotiationId): Lang#PriorityRaiseRequest                                   = spec.start.get apply neg
}
