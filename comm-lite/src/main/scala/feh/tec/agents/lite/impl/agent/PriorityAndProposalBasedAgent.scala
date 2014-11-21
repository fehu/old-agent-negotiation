package feh.tec.agents.lite.impl.agent

import akka.actor.ActorLogging
import feh.tec.agents.lite.AgentCreationInterface.NegotiationInit
import feh.tec.agents.lite.impl.ConstraintsSatisfactionChecks
import feh.tec.agents.lite.spec.AgentSpecification
import feh.tec.agents.lite._
import feh.tec.agents.lite.spec.AgentSpecification.PriorityAndProposalBased
import feh.util._
import feh.tec.agents.lite.impl

abstract class PriorityAndProposalBasedAgent[Lang <: Language.ProposalBased with Language.HasPriority](
            val uniqueName: String,
            override val role: NegotiationRole,
            val negotiationsInit: Set[NegotiationInit],
            val args: AgentCreationInterface#Args
          )
  extends impl.PriorityAndProposalBasedAgent[Lang] with AgentCreationInterface with ResponseDelay[Lang]
{
  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang]
  type Agent <: PriorityAndProposalBasedAgent[Lang]

  val spec: AgentSpecification.PriorityAndProposalBased[Agent, Lang]

  implicit def owner: Agent = this.asInstanceOf[Agent]

  /* should be defined by ExtendableDefinition */

  def nothingToPropose(neg: NegotiationId): Unit            = spec.nothingToPropose.get apply neg
  protected def beforeEachMessage(msg: Lang#Msg): Unit      = spec.beforeEachMessage.get apply msg
  def setNextProposal(neg: NegotiationId): Lang#Proposal    = spec.setNextProposal.get apply neg
  def nextValues(neg: NegotiationId): Option[Map[Var, Any]] = spec.nextValues.get apply neg
  def onProposal: PartialFunction[Lang#Proposal, Any]       = spec.onProposal.get
  def onRejection: PartialFunction[Lang#Rejection, Any]     = spec.onRejection.get
  def onAcceptance: PartialFunction[Lang#Acceptance, Any]   = spec.onAcceptance.get
  def moreProcess: PartialFunction[Lang#Msg, Any]           = spec.moreProcess.get
  def processUserMessage: PartialFunction[UserMessage, Any] = spec.processUserMessage.get
  def updateCurrentProposal(neg: NegotiationId): Unit       = spec.updateCurrentProposal.get apply neg
  def stop(): Unit                                          = spec.stop.get
  def reset(): Unit                                         = spec.reset.get
  def start(): Unit                                         = spec.start.get
  def initialize(): Unit                                    = spec.initialize.get

  lazy val negotiationIds: Set[NegotiationId] = negotiationsInit.map(_.id)

//  lazy val priorityNegotiationHandler: PriorityNegotiationHandler[Lang] = new PriorityNegotiationHandlerImpl(
//    spec.priorityNegotiationHandler.get, owner, this.get, this.sendToAll
//  )
}

class PriorityNegotiationHandlerImpl[Owner <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriorityNegotiation](
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

trait NegotiationCreation {
  self: PriorityProposalBasedAgent[_] =>

  def negotiationCreated(neg: Negotiation) = {}
}

@deprecated("is related to a negotiation, not the entire agent")
trait RequiresDistinctPriority extends NegotiationCreation{
  self: PriorityProposalBasedAgent[_] with ActorLogging =>

  def initialPriority: Map[NegotiationId, Priority]

  override def negotiationCreated(neg: Negotiation) = {
    super.negotiationCreated(neg)
    neg.currentPriority update initialPriority(neg.id)
  }
}
