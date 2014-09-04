package feh.tec.agents

import java.util.UUID

import akka.actor.{Actor, ActorRef}

import scala.collection.mutable

trait AbstractAgent extends Actor{
  type Id

  val id: Id
  implicit val ref: AgentRef

  def lifeCycle: PartialFunction[AbstractMessage, Unit]

  def receive = {
    case msg: AbstractMessage =>
      _currentMsg = msg
      lifeCycle(msg)
  }

  def currentMsg = _currentMsg
  protected var _currentMsg: AbstractMessage = null
}

trait AgentRef{
  def id: impl.Agent.Id
  def ref: ActorRef
//  def !(msg: AbstractMessage)

  override def toString: String = s"Agent($id)"
}

class Priority(val get: Int) extends AnyVal{
  def raise(amount: Int = 1) = new Priority(get + amount)
}
object Priority{
  implicit def priorityToIntWrapper(p: Priority) = p.get
}

case class NegotiationId(name: String, id: UUID = UUID.randomUUID())

trait Negotiation{
  def id: NegotiationId

  def priority = currentPriority
  implicit var currentPriority: Priority

  def scope: Set[AgentRef]

  val currentValues: mutable.HashMap[Var, Any]
}

trait WeakCommitmentNegotiation extends Negotiation{
  var currentIssues: Set[Var]
  var failedConfigurations: Set[Map[Var, Any]]
}

trait Role {
  val name: String

  override def equals(obj: scala.Any) = obj match {
    case that: Role => that.name == this.name
    case _ => false
  }

  override def toString = s"Role($name)"
}

object Role{
  def apply(nme: String) = new Role{ val name: String = nme }
}

protected[agents] trait SystemRole extends Role{ override def toString = s"SystemRole($name)" }
trait UserRole extends Role{ override def toString = s"UserRole($name)" }

trait NegotiatingAgent extends AbstractAgent{
  val role: Role

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

  def createProposal(id: NegotiationId): Lang#Proposal
  def createRejected(id: NegotiationId): Lang#Rejected
  def createAccepted(id: NegotiationId): Lang#Accepted

  def onProposal: PartialFunction[Lang#Proposal, Unit]
  def onRejected: PartialFunction[Lang#Rejected, Unit]
  def onAccepted: PartialFunction[Lang#Accepted, Unit]

  def process = {// type test might fail
    case msg: Lang#Proposal if lang.isProposal(msg)    => onProposal(msg)
    case msg: Lang#Accepted if lang.isAcceptance(msg)  => onAccepted(msg)
    case msg: Lang#Rejected if lang.isRejection(msg)   => onRejected(msg)
  }
}

@deprecated("use PriorityBasedNegotiatingAgent instead")
trait FallbackBackTracking[Lang <: BacktrackLanguage] extends ProposalBased[Lang]{
  self: NegotiatingAgent =>

  def createFallback(negotiation: Negotiation): Lang#Proposal

  def onFallback(msg: Lang#Fallback)

  override def process = super.process orElse  {
    case msg: Lang#Fallback if lang.isFallback(msg) => onFallback(msg)
  }
}

trait InfoGathering extends AbstractAgent{
  /**
   * WARNING: currentMessage will show the previous one;
   * anyway, the actual gathering is supposed to be performed externally
   */
  def gatherInfo(msg: AbstractMessage)

  override def receive = {
    case msg: AbstractMessage =>
      gatherInfo(msg)
      super.receive(msg)
  }
}
