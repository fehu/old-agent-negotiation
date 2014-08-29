package feh.tec.agents.impl

import java.util.UUID

import akka.actor.Props
import feh.tec.agents.{AgentRef, service}

object System {

  def conflictResolver = Props(classOf[ConflictResolver])

  class ConflictResolver extends service.ConflictResolver{
    type Id = Agent.Id
    val id = new Agent.Id(service.ConflictResolver.Role, UUID.randomUUID())
    implicit val ref = AgentRef(id, self)
  }

}
