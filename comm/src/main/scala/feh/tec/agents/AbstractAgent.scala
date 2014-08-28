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
  protected var _currentMsg: AbstractMessage = null
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

trait NegotiatingAgent extends AbstractAgent{
  implicit val ref: AgentRef

  val role: Role
  val vars: Set[Var]

  type ANegotiation <: Negotiation
  def negotiations: Set[ANegotiation]

}

trait SpeakingAgent[Lang <: Language]{
  self: NegotiatingAgent =>

  val lang: Lang

  def processSys: PartialFunction[SystemMessage, Unit]
  def process: PartialFunction[Lang#Msg, Unit]

  def lifeCycle: PartialFunction[AbstractMessage, Unit] = {
    case sys: SystemMessage => processSys(sys)
    case msg: Lang#Msg if lang.isMessage(msg) => process(msg)
  }
}

trait ProposalBased[Lang <: ProposalLanguage] extends SpeakingAgent[Lang]{
  self: NegotiatingAgent =>

  def createProposal(negotiation: Negotiation): Lang#Proposal
  def createRejected(negotiation: Negotiation): Lang#Rejected
  def createAccepted(negotiation: Negotiation): Lang#Accepted

  def onProposal(msg: Lang#Proposal)
  def onRejected(msg: Lang#Rejected)
  def onAccepted(msg: Lang#Accepted)

  def process = {// type test might fail
    case msg: Lang#Proposal if lang.isProposal(msg)    => onProposal(msg)
    case msg: Lang#Accepted if lang.isAcceptance(msg)  => onAccepted(msg)
    case msg: Lang#Rejected if lang.isRejection(msg)   => onRejected(msg)
  }
}

trait BackTracking[Lang <: BacktrackLanguage] extends ProposalBased[Lang]{
  self: NegotiatingAgent =>

  def createFallback(negotiation: Negotiation): Lang#Proposal

  def onFallback(msg: Lang#Fallback)

  override def process = super.process orElse  {
    case msg: Lang#Fallback if lang.isFallback(msg) => onFallback(msg)
  }
}
