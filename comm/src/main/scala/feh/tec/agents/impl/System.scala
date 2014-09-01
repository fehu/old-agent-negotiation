package feh.tec.agents.impl

import java.util.UUID

import akka.actor.Props
import feh.tec.agents.{SystemRole, AbstractAgent, service}

object System {
  object Service{
    sealed trait ArgsArity{ self: AbstractAgent => }

    trait Args0 extends ArgsArity{ self: AbstractAgent => }
    trait Args1[T] extends ArgsArity{ self: AbstractAgent => }
  }

  def conflictResolver = Props(classOf[ConflictResolver])

  class ConflictResolver extends SystemAgent with service.ConflictResolver with Service.Args0
}

trait SystemAgent extends AbstractAgent{
  def role: SystemRole

  type Id = Agent.Id
  val id = Agent.Id(role, UUID.randomUUID())
  implicit val ref = AgentRef(id, self)
}