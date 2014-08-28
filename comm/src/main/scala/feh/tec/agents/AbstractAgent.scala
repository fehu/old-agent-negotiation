package feh.tec.agents

import java.util.UUID

import akka.actor.Actor

import scala.collection.mutable

trait AbstractAgent extends Actor{
  type Id

  val id: Id

  def lifeCycle: PartialFunction[AbstractMessage, Unit]

  def receive = {
    case msg: AbstractMessage =>
      _currentMsg = msg
      lifeCycle(msg)
  }

  def currentMsg = _currentMsg
  protected var _currentMsg: AbstractMessage
}

class Priority(val get: Int) extends AnyVal
object Priority{
  implicit def priorityToIntWrapper(p: Priority) = p.get
}

case class NegotiationId(name: String, id: UUID = UUID.randomUUID())

trait Negotiation{
  def id: NegotiationId

  implicit var currentPriority: Priority

  def scope: Set[AgentRef]

  val vals: mutable.HashMap[Var, Any]
}

trait WeakCommitmentNegotiation extends Negotiation{
  var currentIssues: Set[Var]
  var failedConfigurations: Set[Map[Var, Any]]
}

trait Role {
  val name: String
}

trait NegotiatingAgent[Lang <: Language] extends AbstractAgent{
  implicit val ref: AgentRef

  val role: Role
  val vars: Set[Var]

  def negotiations: Set[Negotiation]

  def process(sys: SystemMessage)
  def process(p: Message)

  def lifeCycle = {
    case sys: SystemMessage => process(sys)
    case msg: Message => process(msg)
  }
}


trait ProposalBased[Lang <: ProposalLanguage] {
  self: NegotiatingAgent[Lang] =>

  def createProposal(negotiation: Negotiation): Lang#Proposal
  def createRejected(negotiation: Negotiation): Lang#Rejected
  def createAccepted(negotiation: Negotiation): Lang#Accepted

  def onProposal(msg: Lang#Proposal)
  def onRejected(msg: Lang#Rejected)
  def onAccepted(msg: Lang#Accepted)
}

trait BackTracking[Lang <: BacktrackLanguage] extends ProposalBased[Lang]{
  self: NegotiatingAgent[Lang] =>

  def createFallback(negotiation: Negotiation): Lang#Proposal

  def onFallback(msg: Lang#Fallback)
}

trait StaticScope{
  self: Negotiation =>

  val scope: Set[AgentRef]
}

trait DynamicScope{
  self: Negotiation =>

  protected def scopeProvider: () => Set[AgentRef]

  final def scope = scopeProvider()
}
