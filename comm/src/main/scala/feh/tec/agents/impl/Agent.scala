package feh.tec.agents.impl

import java.util.UUID

import akka.actor.ActorRef
import feh.tec.agents.Message.Response
import feh.tec.agents.SystemMessage.{RefDemand, ReportAllStates, ReportStates, ScopeUpdate}
import feh.tec.agents._
import feh.tec.agents.impl.Language.dsl._
import feh.tec.agents.impl.Negotiation.DynamicScope
import feh.tec.agents.impl.agent.AgentCreation.NegotiationInit
import feh.util._
import scala.collection.mutable

object Agent{
  case class Id(role: Role, uuid: UUID)
  trait IdNamed extends Id{
    def name: String

    override def toString: String = s"Id($name, $role, $uuid)"
  }
  object Id{
    def withName(role: Role, uuid: UUID, nme: String): IdNamed = new Id(role, uuid) with IdNamed{ def name = nme }
  }

  trait EssentialSystemSupport{
    self: AbstractAgent =>

    // RefDemand Messages are essential for agent creation
    def processSys: PartialFunction[SystemMessage, Unit] = {
      case RefDemand() => sender() ! ref
    }
  }

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

  trait SystemSupport extends HasStatus with EssentialSystemSupport{
    self: SpeakingAgent[_] with AgentHelpers[_] =>

    def initReady_? = true
    protected def updateInitStatus() =
      if((status == Status.Created || status == Status.Initializing) && initReady_?) status = Status.Initialized

    def startLife()

    override def processSys: PartialFunction[SystemMessage, Unit] = super.processSys orElse {
// Start messages
      case start: SystemMessage.Start if status == Status.Initialized || status == Status.Created && initReady_? =>
        startLife()
        status = Status.Working
        sender() ! start.done
      case start: SystemMessage.Start if status == Status.Created => sender() ! start.notInitialized
      case start: SystemMessage.Start if status == Status.Initializing =>
        sender() ! start.stillInitializing
        updateInitStatus()
      case start: SystemMessage.Start if status == Status.Working => sender ! start.alreadyRunning
// ReportState Messages
      case req: ReportStates => sender ! req.response(getOpt _ andThen{ _.map{
        neg => (neg.priority, neg.currentValues.toMap, neg.scope, extractReportExtra(neg.id))
      }})
      case req: ReportAllStates => sender ! req.response(negotiations.map{
        neg => neg.id -> (neg.priority, neg.currentValues.toMap, neg.scope, extractReportExtra(neg.id))
      }.toMap)
    }

    protected def extractReportExtra(negId: NegotiationId): Option[Any] = None
  }

  /** Registers proposals sent using AgentRef.! method to provide `expectingResponse` method of [[ProposalRegister]],
   *  removes them from the register after onAccepted/onRejected execution
   *  expects the default AgentRef implementation is used
   *  don't forget to use discard if a proposal is discarded to preserve memory (not all agents may respond)
   */
  trait ProposalRegistering[Lang <: ProposalLanguage] extends ProposalBased[Lang] with ProposalRegister[Lang]{
    self: NegotiatingAgent =>

    protected var proposalsWithoutResponse = mutable.HashSet.empty[(Message.Id, AgentRef)]

    def expectingResponse(msg: Lang#Msg) = proposalsWithoutResponse contains msg.id -> msg.sender
    def discardProposal(id: Message.Id) = proposalsWithoutResponse = proposalsWithoutResponse.filter(_._1 != id)
    def registerProposal(msg: Lang#Proposal, to: AgentRef) = proposalsWithoutResponse += msg.id -> to

    override def lifeCycle = AgentRef.withOnSendHook{
      case (msg, to) if lang.isProposal(msg) => proposalsWithoutResponse += msg.id -> to
      case (_, _) => // do nothing
    }(super.lifeCycle)


    abstract override def onRejected =  execAndMarkRespondedAfter(super.onRejected)
    abstract override def onAccepted = execAndMarkRespondedAfter(super.onAccepted)

    private def execAndMarkRespondedAfter[Expr <: Response](behaviorFunc: PartialFunction[Expr, Unit]): PartialFunction[Expr, Unit] = {
      case msg if behaviorFunc.isDefinedAt(msg) =>
        behaviorFunc(msg)
        proposalsWithoutResponse -= msg.respondingTo -> msg.sender
    }
  }

}

object AgentRef{
  type OnSendHook = (AbstractMessage, AgentRef) => Unit
  private object OnSendHook extends ScopedState[(AbstractMessage, AgentRef) => Unit]((_, _) => {})

  def withOnSendHook[R](hook: OnSendHook)(f: => R) = OnSendHook.doWith(hook)(f)

  def apply(_id: impl.Agent.Id, _ref: ActorRef) =
    new AgentRef{
      def id = _id
      def ref = _ref

      def !(msg: AbstractMessage) = {
        ref ! msg
        OnSendHook.get(msg, this)
      }
    }
}

trait DynamicScopeSupport extends Agent.SystemSupport with NegotiatingAgent{
  self: SpeakingAgent[_] with AgentHelpers[_] =>

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

abstract class Agent[Lang <: ProposalLanguage]() extends NegotiatingAgent
  with ProposalBased[Lang] with Language.Builder[Lang] with DynamicScopeSupport with AgentHelpers[Lang]
{
  type Id = Agent.Id
  final implicit val ref = AgentRef(id, self)

  def startLife() = negotiations.foreach(neg => createProposal(neg.id) $$ (sendToAll(neg, _)))

  def createProposal(id: NegotiationId) = build[Lang#Proposal](get(id), I propose I set (
    get(id).currentValues.toSeq map {
      case (issue, value) => issue -> issue.cast(value).getOrThrow(s"wrong value $value for $issue")
    } : _*))
  def createAccepted(id: NegotiationId) = buildMessage(id, I.accept).asInstanceOf[Lang#Accepted]
  def createRejected(id: NegotiationId) = buildMessage(id, I.reject).asInstanceOf[Lang#Rejected]

  private def build[T <: Lang#Msg](neg: Negotiation, expr: Language.Buildable) =
    buildMessage(neg.id, expr).asInstanceOf[T]
}

trait NegotiationSupport{
  type ANegotiation <: Negotiation

  def createNegotiation(id: NegotiationId, init: NegotiationInit): ANegotiation
}
object NegotiationSupport{
  trait Default extends NegotiationSupport{

    type ANegotiation = DynamicScopeNegotiation
    def createNegotiation(id: NegotiationId, init: NegotiationInit) =
      new DynamicScopeNegotiation(id, init.priority, init.issues)
  }
}