package feh.tec.agents.light

import akka.actor.Actor.Receive
import akka.actor._

trait AbstractAgent extends Actor{
  val name: String
  val role: Role
}

trait SystemAgent extends AbstractAgent{
  override val role: SystemRole
}

case class Role(name: String)
trait NegotiationRole extends Role
object NegotiationRole{ def apply(name: String): NegotiationRole = new Role(name) with NegotiationRole }
trait SystemRole extends Role
object SystemRole{ def apply(name: String): SystemRole = new Role(name) with SystemRole }

case class AgentRef(id: Agent.Id, protected[light] val ref: ActorRef)

trait SpeakingAgent[Lang <: Language] extends AbstractAgent{
  implicit val ref: AgentRef
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
  def process: PartialFunction[Lang#Priority, Any]

  def start(neg: NegotiationId): Lang#PriorityRaiseRequest
  def decide(requests: Map[AgentRef, Lang#PriorityRaiseRequest]): Lang#PriorityRaiseResponse
  def onPriorityUpdate(f: (NegotiationId, Option[Priority])): Any
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