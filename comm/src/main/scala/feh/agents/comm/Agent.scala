package feh.agents.comm

import akka.actor.{ActorRef, Actor}
import java.util.UUID
import feh.agents.comm.Message.MessageId
import scala.util.Try

/** An abstract agent
 */
trait Agent extends Actor{
  def isSystem: Boolean
  def id: AgentId
}

trait AgentId{
  def ref: ActorRef
}

trait NegotiatingAgentId extends AgentId{
  def role: NegotiationRoleMeta
}

trait Message{
  val id: Message.MessageId
}

object Message{
  type MessageId = UUID

  trait AutoId extends Message{
    val id = UUID.randomUUID()
  }
}

trait NegotiatingRole[Lang <: Language] {
  self: Agent =>

  def language: Lang
  def iSpace: IssuesSpace
  def coordinator: NegotiationCoordinator

  def isSystem = false

  def id: NegotiatingAgentId
  def meta: NegotiationRoleMeta //= id.role
}

trait SystemRole {
  self: Agent =>

  def isSystem = true
}

trait NegotiationRoleMeta{
  def name: String
}

trait NegotiationSchema{
  def roles: Set[NegotiationRoleMeta]
}

/** Defines Request and Response for companion objects of SystemRole implementations
 */
trait SystemRoleCompanion{
  trait Request extends Message.AutoId{
    implicit def sender: AgentId
  }

  trait Response extends Message.AutoId{
    def requestId: MessageId
  }
}

object NegotiationCoordinator extends SystemRoleCompanion{
  trait FindDSL{
    type Query
  }

  case class Register(implicit val sender: AgentId) extends Request
  case class Find[DSL <: FindDSL](query: DSL#Query)(implicit val sender: AgentId, val dsl: DSL) extends Request


  case class Registered(requestId: MessageId) extends Response
  case class Found(result: Try[Seq[AgentId]], requestId: MessageId)
}

trait NegotiationCoordinator extends Agent with SystemRole{
  def schema: NegotiationSchema
}
