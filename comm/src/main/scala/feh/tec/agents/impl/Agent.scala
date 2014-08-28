package feh.tec.agents.impl

import java.util.UUID
import Language.dsl._
import feh.tec.agents
import feh.tec.agents.SystemMessage.ScopeUpdate
import feh.tec.agents.impl.Negotiation.DynamicScope
import feh.util._

import feh.tec.agents._

object Agent{
  case class Id(role: Role, uuid: UUID)

  sealed trait Status
  object Status{
    case object Created extends Status
    case object Initializing extends Status
    case object Initialized extends Status

    case object Working extends Status
    case object Exception extends Status
    case object Stopped extends Status
  }

  trait HasStatus extends NegotiatingAgent{
    self: SpeakingAgent[_] =>

    var status: Status = Status.Created
  }

  trait SystemSupport extends HasStatus{
    self: SpeakingAgent[_] =>

    def initReady_? = true
    protected def updateInitStatus() =
      if((status == Status.Created || status == Status.Initializing) && initReady_?) status = Status.Initialized

    def startLife()

    def processSys: PartialFunction[SystemMessage, Unit] = {
      case start: SystemMessage.Start if status == Status.Initialized || status == Status.Created && initReady_? =>
        startLife()
        status = Status.Working
        start.sender.ref ! start.done
      case start: SystemMessage.Start if status == Status.Created => start.sender.ref ! start.notInitialized
      case start: SystemMessage.Start if status == Status.Initializing =>
        start.sender.ref ! start.stillInitializing
        updateInitStatus()
      case start: SystemMessage.Start if status == Status.Working => start.sender.ref ! start.alreadyRunning
    }
  }

  trait Helpers[Lang <: Language]{
    self: NegotiatingAgent with SpeakingAgent[Lang] =>

    def sendToAll(neg: Negotiation, msg: Lang#Msg) = neg.scope.foreach(_ ! msg)

    protected var negotiationsCache = negotiations.map(n => n.id -> n).toMap
    def get(neg: NegotiationId) = negotiationsCache(neg)
  }
}

trait DynamicScopeSupport extends Agent.SystemSupport with NegotiatingAgent{
  self: SpeakingAgent[_] =>

  type ANegotiation <: Negotiation with DynamicScope

  private var scopesInited = negotiations.map(_.id -> false).toMap
  override def initReady_? = super.initReady_? && scopesInited.forall(_._2)

  override def processSys = super.processSys orElse {
    case upd: ScopeUpdate if negotiations.exists(_.id == upd.negotiation) =>
      negotiations.find(_.id == upd.negotiation).map(_.updateScope(upd))
      scopesInited += upd.negotiation -> true
      if (status == Agent.Status.Initializing) updateInitStatus()
  }
}

abstract class Agent[Lang <: BacktrackLanguage]() extends NegotiatingAgent
  with ProposalBased[Lang] with Language.Builder[Lang] with DynamicScopeSupport with Agent.Helpers[Lang]
{
  type Id = Agent.Id
  final implicit val ref = AgentRef(id, self)

  def startLife() = negotiations.foreach(neg => createProposal(neg.id) $$ (sendToAll(neg, _)))

  def createProposal(id: NegotiationId) = build[Lang#Proposal](get(id), I propose I set (
    get(id).vals.toSeq map {
      case (issue, value) => issue -> issue.cast(value).getOrThrow(s"wrong value $value for $issue")
    } : _*))
  def createAccepted(id: NegotiationId) = buildMessage(id, I.accept).asInstanceOf[Lang#Accepted]
  def createRejected(id: NegotiationId) = buildMessage(id, I.reject).asInstanceOf[Lang#Rejected]

  private def build[T <: Lang#Msg](neg: Negotiation, expr: Language.Buildable) =
    buildMessage(neg.id, I.accept).asInstanceOf[T]
}

// intended to be used with Views
trait PriorityBasedBacktrackAgent[Lang <: BacktrackLanguage] extends Agent[Lang]
{
//  type ANegotiation = DynamicScopeNegotiation

  protected def accept(prop: Lang#Proposal): Boolean

  def onProposal = {
    case prop: Lang#Proposal if lang.isProposal(prop) && prop.priority == get(prop.negotiation).priority => //todo
      
    case prop: Lang#Proposal if lang.isProposal(prop) && prop.priority < get(prop.negotiation).priority =>
      prop.sender ! (if(accept(prop)) createAccepted(prop.negotiation) else createRejected(prop.negotiation))
  }
  def onRejected = {
    case msg: Lang#Rejected if lang.isRejection(msg) && msg.priority == get(msg.negotiation).priority => //todo 
    case msg: Lang#Rejected if lang.isRejection(msg) && msg.priority > get(msg.negotiation).priority => 
      decideOnRejection(msg)
  }
  // do nothing for now
  def onAccepted = Map()

  def decideOnRejection(msg: Lang#Rejected)
}
