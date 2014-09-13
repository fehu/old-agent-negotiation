package feh.tec.agents.impl

import java.util.UUID

import akka.actor.{ActorLogging, ActorRef}
import feh.tec.agents.Message.Response
import feh.tec.agents.SystemMessage.{RefDemand, ScopeUpdate}
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

    object named{
      def unapply(id: Id): Option[(String, Role, UUID)] = PartialFunction.condOpt(id){
        case named: IdNamed => (named.name, named.role, named.uuid)
      }
    }
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

  trait SystemSupport extends HasStatus with EssentialSystemSupport with AbstractAgent{
    self: SpeakingAgent[_] with AgentHelpers[_] =>

    def initReady_? = true
    protected def updateInitStatus() =
      if((status == Status.Created || status == Status.Initializing) && initReady_?) status = Status.Initialized

    def startLife()
    def resumeLife()

    override def receive =  ({
      case msg if status != Status.Working && !msg.isInstanceOf[SystemMessage] => // do nothing
    }: PartialFunction[Any, Unit]) orElse super.receive

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
// Stop messages
      case stop@SystemMessage.Stop() =>
        status = Status.Stopped
        sender ! stop.stopped
// Resume messages
      case resume@SystemMessage.Resume() if status == Status.Stopped =>
        resumeLife()
        status = Status.Working
        sender ! resume.resumed
// ReportState messages
      case req: AgentReports.ReportStates => sender ! req.response(getOpt _ andThen{ _.map{
        neg => (neg.priority, neg.currentValues.toMap, neg.scope, extractReportExtra(neg.id))
      }})
      case req: AgentReports.ReportAllStates => sender ! reportAllStates(req.id)
    }

    protected def reportAllStates(id: UUID = UUID.randomUUID()) = {
      val negs = negotiations.map{
        neg => neg.id -> AgentReports.StateReportEntry(neg.priority, neg.currentValues.toMap, neg.scope, extractReportExtra(neg.id))
      }.toMap
      AgentReports.StateReport(ref, negs, id)
    }

    protected def extractReportExtra(negId: NegotiationId): Option[Any] = None
  }

  /** Registers proposals sent using AgentRef.! method to provide `expectingResponse` method of [[ProposalRegister]],
   *  removes them from the register after onAccepted/onRejected execution
   *  expects the default AgentRef implementation is used
   *  don't forget to use discard if a proposal is discarded to preserve memory (not all agents may respond)
   */
  trait ProposalRegistering[Lang <: ProposalLanguage] extends ProposalBased[Lang] with ProposalRegister[Lang]{
    self: NegotiatingAgent with AgentHelpers[Lang] =>

    protected var proposalsWithoutResponse = mutable.HashSet.empty[(Message.Id, AgentRef)]

    def expectingResponse(msg: Lang#Msg) = proposalsWithoutResponse contains msg.id -> msg.sender
    def discardProposal(id: Message.Id) = proposalsWithoutResponse = proposalsWithoutResponse.filter(_._1 != id)
    def registerProposal(msg: Lang#Proposal, to: AgentRef) = proposalsWithoutResponse += msg.id -> to

    override def lifeCycle = {
      case m if super.lifeCycle isDefinedAt m =>
        hooks.OnSend.withHooks{
          case (to, msg) if lang.isProposal(msg) => proposalsWithoutResponse += msg.id -> to
          case (_, _) => // do nothing
        }(super.lifeCycle(m))
    }


    abstract override def onRejected =  execAndMarkRespondedAfter(super.onRejected)
    abstract override def onAccepted = execAndMarkRespondedAfter(super.onAccepted)

    private def execAndMarkRespondedAfter[Expr <: Response](behaviorFunc: PartialFunction[Expr, Unit]): PartialFunction[Expr, Unit] = {
      case msg if behaviorFunc.isDefinedAt(msg) =>
        behaviorFunc(msg)
        proposalsWithoutResponse -= msg.respondingTo -> msg.sender
    }
  }

  trait AgentReporting[Lang <: Language] extends SpeakingAgent[Lang] with SystemSupport{
    self: NegotiatingAgent with AgentHelpers[Lang] =>

    def reportingTo: AgentRef

    override def lifeCycle: PartialFunction[AbstractMessage, Unit] = {
      case m if super.lifeCycle isDefinedAt m =>
        hooks.OnSend.withHooks{
          case (to, msg) if lang.isMessage(msg) =>
            buildReports(msg.asInstanceOf[Lang#Msg], to) foreach reportingTo.ref.!
          case msg => // ignore system messages
        }(super.lifeCycle(m))
    }

    protected def buildReports: (Lang#Msg, AgentRef) =>  Set[AgentReport]
  }

  trait AgentReportingMessagesAndState[Lang <: Language] extends AgentReporting[Lang]{
    self: NegotiatingAgent with AgentHelpers[Lang] =>

    protected def buildReports = (msg, to) =>  Set(
      AgentReports.MessageReport(to, msg),
      reportAllStates()
    )

  }
}

trait ActorHooks{
  def send: Set[(AgentRef, AbstractMessage) => Unit]
}

object AgentRef{

  def apply(_id: impl.Agent.Id, _ref: ActorRef) =
    new AgentRef{
      def id = _id
      def ref = _ref
    }

  def unapply(ref: AgentRef): Option[(impl.Agent.Id, ActorRef)] = Some(ref.id -> ref.ref)
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