package feh.tec.agents.impl

import java.util.UUID

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import feh.tec.agents
import feh.tec.agents.impl.agent.AgentCreation.NegotiationInit
import feh.tec.agents.service.ConflictResolver
import feh.tec.agents._
import feh.tec.agents.impl.agent.{Tuple0, AgentBuilder}
import akka.pattern.ask
import feh.util._

object NegotiationController{
  case object Role extends SystemRole{ val name: String = "Negotiation Controller" }

  /** all agent are created in the beginning, therefore 'static' */
  class StaticAgentsInit(systemAgentBuilders: Map[AgentBuilder[_, _], Int],
                         agentBuilders: Map[AgentBuilder[_, _], (Map[NegotiationId, Set[Var]], Int)],
                         resolveConflictTimeout: Timeout,
                         startTimeout: Timeout,
                         creationTimeout: Timeout)
  extends NegotiationController with SystemAgent
  {
    implicit def acSys = context.system
    implicit def exContext = context.dispatcher

    lazy val systemAgents = {
      implicit def timeout = creationTimeout

      systemAgentBuilders.toSeq.par.flatMap{
        case (b@AgentBuilder.SystemArgs0Service, count) => for(_ <- 1 to count) yield b.create(Tuple0)
      }.toList
    }

    lazy val agents = {
      implicit def timeout = creationTimeout

      agentBuilders.toSeq.par.flatMap{
        case (b@AgentBuilder.Default, (issuesByNeg, count)) =>
          val init = issuesByNeg.mapValues(negotiationInit)
          for(_ <- 1 to count) yield b.create((UUID.randomUUID(), init))
        case (b@AgentBuilder.PriorityBased, (issuesByNeg, count)) =>
          val init = issuesByNeg.mapValues(negotiationInit)
          for(_ <- 1 to count) yield b.create((UUID.randomUUID(), init, conflictResolver, resolveConflictTimeout))
      }.toList
    }

    def role = Role

    def start() = {
      implicit def timeout = startTimeout
      systemAgents // init lazy
      agents map (_.ref ? SystemMessage.Start |> (_.mapTo[SystemMessage] map handleStartup))
    }

    def handleStartup: PartialFunction[SystemMessage, Unit] = {
      case _: SystemMessage.Start.Started => // it's ok
    }

    def stop() = ???

    def conflictResolver = systemAgents.filter(_.id.role == ConflictResolver.Role).ensuring(_.size == 1).head

    def lifeCycle = {
      case _: SystemMessage.Start => start()
    }

    protected def negotiationInit(issues: Set[Var]) = NegotiationInit(nextPriority(), issues)

    protected def nextPriority() = {
      val c = priorityCount
      priorityCount += 1
      new Priority(c)
    }
    private var priorityCount = 0
  }

}

