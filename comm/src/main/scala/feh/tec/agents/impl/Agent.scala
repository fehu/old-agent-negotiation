package feh.tec.agents.impl

import java.util.UUID
import Language.dsl._
import akka.actor.Props
import feh.tec.agents.SystemMessage.ScopeUpdate
import feh.tec.agents.impl.Negotiation.DynamicScope
import scala.reflect
import feh.util._

import feh.tec.agents._

import scala.reflect.ClassTag

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

abstract class Agent[Lang <: ProposalLanguage]() extends NegotiatingAgent
  with ProposalBased[Lang] with Language.Builder[Lang] with DynamicScopeSupport with AgentHelpers[Lang]
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

abstract class AgentCreation[Lang <: ProposalLanguage](uuid: UUID,
                                                       negotiationInit: Map[NegotiationId, AgentCreation.NegotiationInit])
  extends Agent[Lang]
{
  def createNegotiation(id: NegotiationId, init: AgentCreation.NegotiationInit): ANegotiation

  val id = Agent.Id(role, uuid)
  lazy val negotiations = negotiationInit.toSet.map((createNegotiation _).tupled)

}

object AgentCreation{
  type Interface = (UUID, Map[NegotiationId, NegotiationInit])

  def props[Ag <: AgentCreation[_] : ClassTag](uuid: UUID,
                                               negotiationInit: Map[NegotiationId, AgentCreation.NegotiationInit]) =
    Props(reflect.classTag[Ag].runtimeClass, uuid, negotiationInit)

  trait NegotiationInit{
    def priority: Priority
    def values: Map[Var, Any]
  }

  type DefaultNegInit = (Priority, Map[Var, Any])

  trait NegotiationInitBuilder[-Ag <: Agent[_], Args]{
    def build(args: Args): NegotiationInit
  }

  implicit object DefaultNegotiationInit extends NegotiationInitBuilder[AgentCreation[_], DefaultNegInit]{
    def build(args: (Priority, Map[Var, Any])) =
      new NegotiationInit {
        def values = args._2
        def priority = args._1
      }
  }

}
