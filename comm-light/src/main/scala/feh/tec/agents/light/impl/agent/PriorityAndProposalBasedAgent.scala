package feh.tec.agents.light.impl.agent

import feh.tec.agents.light.AgentCreationInterface.NegotiationInit
import feh.tec.agents.light.impl.ConstraintsSatisfactionChecks
import feh.tec.agents.light.spec.AgentSpecification
import feh.tec.agents.light._
import feh.tec.agents.light.spec.AgentSpecification.PriorityAndProposalBased
import feh.util._
import feh.tec.agents.light.impl

abstract class PriorityAndProposalBasedAgent[Lang <: Language.ProposalBased with Language.HasPriority](
            val uniqueName: String,
            override val role: NegotiationRole,
            val negotiationsInit: Set[NegotiationInit],
            val args: AgentCreationInterface#Args
          )
  extends impl.PriorityAndProposalBasedAgent[Lang] with AgentCreationInterface
{
  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang]
  type Agent <: PriorityAndProposalBasedAgent[Lang]

  val spec: AgentSpecification.PriorityAndProposalBased[Agent, Lang]

  implicit def owner: Agent = this.asInstanceOf[Agent]

  log.debug("negotiationsInit = " + negotiationsInit)

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

  lazy val negotiationIds: Set[NegotiationId] = negotiationsInit.map(_.id)

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
  extends PriorityNegotiationHandler[Lang]
{
  def updatePriority(neg: NegotiationId, responses: Map[AgentRef, Lang#PriorityRaiseResponse]): Unit              = spec.onPriorityUpdate.get apply (neg, responses)
  def decide(neg: NegotiationId, requests: Map[AgentRef, Lang#PriorityRaiseRequest]): Lang#PriorityRaiseResponse  = spec.decide.get apply neg apply requests
  def start(neg: NegotiationId): Lang#PriorityRaiseRequest                                                        = spec.start.get apply neg

  def process: PartialFunction[Lang#Priority, Any]                                                                = spec.process.get
}

@deprecated("is related to a negotiation, not the entire agent")
trait RequiresDistinctPriority{
  self: PriorityProposalBasedAgent[_] =>

  def initialPriority: Map[NegotiationId, Priority]
}
