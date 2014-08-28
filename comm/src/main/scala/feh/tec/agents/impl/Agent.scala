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


// todo separate
trait PriorityBasedBacktrackAgent[Lang <: BacktrackLanguage]
  extends NegotiatingAgent with BackTracking[Lang] with Language.Builder[Lang] with DynamicScopeSupport
{
  type Id = Agent.Id
  type ANegotiation = DynamicScopeNegotiation
  final implicit val ref = AgentRef(id, self)

  def startLife() = negotiations.foreach{
    neg =>
      val prop = createProposal(neg)
      neg.scope.foreach(_ ! prop)
  }

  def onProposal(msg: Lang#Proposal) = ???
  def onAccepted(msg: Lang#Accepted) = ???
  def onRejected(msg: Lang#Rejected) = ???
  def onFallback(msg: Lang#Fallback) = ???

  def createProposal(negotiation: Negotiation) = build[Lang#Proposal](negotiation, I propose I set (
    negotiation.vals.toSeq map {
      case (issue, value) => issue -> issue.cast(value).getOrThrow(s"wrong value $value for $issue")
    } : _*))
  def createAccepted(negotiation: Negotiation) = buildMessage(negotiation.id, I.accept).asInstanceOf[Lang#Accepted]
  def createRejected(negotiation: Negotiation) = buildMessage(negotiation.id, I.reject).asInstanceOf[Lang#Rejected]
  def createFallback(negotiation: Negotiation) = ???

  private def build[T <: Lang#Msg](neg: Negotiation, expr: Language.Buildable) =
    buildMessage(neg.id, I.accept).asInstanceOf[T]
}
