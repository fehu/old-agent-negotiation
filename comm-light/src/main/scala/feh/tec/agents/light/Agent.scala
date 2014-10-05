package feh.tec.agents.light

import akka.actor._

trait AbstractAgent extends Actor{
  val name: String
  val role: Role
}

case class Role(name: String)
trait NegotiationRole extends Role

case class AgentRef(id: Agent.Id, ref: ActorRef)

trait SpeakingAgent[Lang <: Language] extends AbstractAgent{
  val ref: AgentRef
  def process: PartialFunction[Lang#Msg, Any]
}

trait NegotiatingAgent[Lang <: NegotiationLanguage] extends SpeakingAgent[Lang]{
  type Negotiation <: AbstractNegotiation

  override val role: NegotiationRole
  def negotiations: Set[Negotiation]
}

trait ProposalBasedAgent[Lang <: Language.ProposalBased] extends NegotiatingAgent[Lang]{
  type Negotiation <: Negotiation.HasProposal[Lang]

  def onProposal: PartialFunction[Lang#Proposal, Any]
  def onAcceptance: PartialFunction[Lang#Acceptance, Any]
  def onRejection: PartialFunction[Lang#Rejection, Any]

  def updateCurrentProposal(neg: NegotiationId)
}

trait PriorityNegotiationHandler[Lang <: Language.HasPriority]{
  def process(msg: Lang#Priority): Any
}

trait PriorityBasedAgent[Lang <: Language.HasPriority] extends NegotiatingAgent[Lang]{
  type Negotiation <: Negotiation.HasPriority

  def requestPriorityRaise(neg: NegotiationId): Lang#PriorityRaiseRequest
  def priorityNegotiationHandler: PriorityNegotiationHandler[Lang]

  def comparePriority(msg: Lang#Msg, f: (Priority, Priority) => Boolean): Boolean
}

trait PriorityProposalBasedAgent[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityBasedAgent[Lang] with ProposalEngine[Lang]
{
  type Negotiation <: Negotiation.HasPriority with Negotiation.HasProposal[Lang]

  def nothingToPropose(neg: NegotiationId)
}

object Agent{
  case class Id(name: String, role: Role)
}