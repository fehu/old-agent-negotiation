package feh.tec.agents.impl

import java.util.UUID

import akka.pattern.ask
import akka.util.Timeout
import feh.tec.agents.Message.AutoId
import feh.tec.agents.NegotiationController.ScopesInitialization
import feh.tec.agents._
import feh.tec.agents.impl.agent.AgentCreation.NegotiationInit
import feh.tec.agents.impl.agent.{AgentBuilder, Tuple0}
import feh.tec.agents.service.ConflictResolver
import feh.util._
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

object NegotiationController{
  object Request{
    case class AgentRefs() extends SystemMessage with AutoId
  }

  case object Role extends SystemRole{ val name = "Negotiation Controller" }

  case class Timeouts(resolveConflict: Timeout, agentCreation: Timeout, agentStartup: Timeout)

  trait GenericBuilding extends NegotiationController with SystemAgent{
    def role = Role

    implicit def acSys = context.system
    implicit def exContext = context.dispatcher

    def timeouts: Timeouts
    protected def systemAgentBuilders: Map[(AgentBuilder[Ag, Arg], ClassTag[Ag]) forSome {type Ag <: AbstractAgent; type Arg <: Product}, Int]

    def handleStartup: PartialFunction[SystemMessage, Unit]

    lazy val systemAgents = {
      systemAgentBuilders.toSeq.par.flatMap{
        case (Tuple2(b: AgentBuilder.SystemArgs0Service, cTag), count) =>
          for(_ <- 1 to count) yield b.create(Tuple0, extracting = true)(cTag, implicitly, timeouts.agentCreation)
      }.toList
    }

    def conflictResolver = systemAgents.filter(_.id.role == ConflictResolver.Role).ensuring(_.size == 1).head

    def start() = {
      implicit def timeout = timeouts.agentStartup
      systemAgents // init lazy
      agents map (_.ref ? SystemMessage.Start |> (_.mapTo[SystemMessage] map handleStartup))
    }

    def stop() = ???

    // todo: stop
    override def lifeCycle = super.lifeCycle orElse {
      case _: SystemMessage.Start => start()
    }

    override def processSys: PartialFunction[SystemMessage, Unit] = super.processSys orElse{
      case req@Request.AgentRefs() => sender() ! agents
    }
  }

  class Counter[T, C](val starting: C, getNext: C => C){

    protected val counts = mutable.HashMap.empty[T, C].withDefaultValue(starting)

    def count(t: T) = counts(t)
    def next(t: T): C = {
      val c = getNext(counts(t))
      counts(t) = c
      c
    }

    def clear() = counts.clear()
  }

  trait PriorityAssignation{
    self: GenericBuilding =>

    def startingPriority: Priority

    lazy val assigningPriority = new Counter[NegotiationId, Priority](startingPriority, _.raise())
  }


  /** all agent are created in the beginning, therefore 'static' */
  @deprecated("does not support scopes!")
  class StaticAgentsInit(protected val systemAgentBuilders: Map[(Tuple2[AgentBuilder[Ag, Arg], ClassTag[Ag]]) forSome {type Ag <: AbstractAgent; type Arg <: Product}, Int],
                         agentBuilders: Map[AgentBuilder[_, _], (Map[NegotiationId, Set[Var]], Int)],
                         val timeouts: Timeouts)
  extends GenericBuilding
  {
    lazy val agents = {
      implicit def timeout = timeouts.agentCreation

      agentBuilders.toSeq.par.flatMap{
        case (b@AgentBuilder.Default, (issuesByNeg, count)) =>
          val init = issuesByNeg.mapValues(negotiationInit)
          for(_ <- 1 to count) yield b.create((UUID.randomUUID(), init), extracting = true)
        case (b@AgentBuilder.PriorityBased, (issuesByNeg, count)) =>
          val init = issuesByNeg.mapValues(negotiationInit)
          for(_ <- 1 to count) yield
            b.create((UUID.randomUUID(), init, conflictResolver, timeouts.resolveConflict), extracting = true)
      }.toList
    }

    def handleStartup: PartialFunction[SystemMessage, Unit] = {
      case _: SystemMessage.Start.Started => // it's ok
    }

    protected def negotiationInit(issues: Set[Var]) = NegotiationInit(nextPriority(), issues)

    protected def nextPriority() = {
      val c = priorityCount
      priorityCount += 1
      new Priority(c)
    }
    private var priorityCount = 0
  }


  object GenericStaticAgentsInit{
    case class AgentInit[BuildAgentArgs, Args <: Product, Ag <: AbstractAgent](
                                               agTag: ClassTag[_ <: Ag],
                                               builder: AgentBuilder[Ag, Args],
                                               buildArgs: BuildAgentArgs => Args,
                                               actorName: String,
                                               scopes: Map[NegotiationId, Set[String]],
                                               count: Int)
    case class Timings(retryToStartAgent: Duration)
  }

  case class GenericStaticInitArgs[BuildAgentArgs](
              systemAgentBuilders: Map[(AgentBuilder[Ag, Arg], ClassTag[Ag]) forSome {type Ag <: AbstractAgent; type Arg <: Product}, Int],
              negotiationIds: Set[NegotiationId],
              agentBuilders: Seq[GenericStaticAgentsInit.AgentInit[BuildAgentArgs, _, _]],
              timeouts: Timeouts,
              timings: GenericStaticAgentsInit.Timings
                                                    )

  abstract class GenericStaticAgentsInit[BuildAgentArgs](arg: GenericStaticInitArgs[BuildAgentArgs])
    extends GenericBuilding with ScopesInitialization with PriorityAssignation
  {
    import GenericStaticAgentsInit._

    protected def buildAgentArgs: BuildAgentArgs

    def systemAgentBuilders = arg.systemAgentBuilders
    def timeouts = arg.timeouts

    lazy val agentsInfo = arg.agentBuilders.flatMap{
      case init@AgentInit(cTag, builder, bArgs, actorName, scopes, count) =>
        for(i <- 1 to count) yield
          builder.create(bArgs(buildAgentArgs), actorName + "-" + i)(cTag, implicitly, timeouts.agentCreation) -> (scopes, count)
    }.toMap

    lazy val agents = agentsInfo.keys.toSeq

    def scopeFor(ag: AgentRef, in: NegotiationId): Set[AgentRef] = agentsInfo(ag)._1(in).flatMap{
      case role => agents.filter(_.id.role == role)
    }

    def handleStartup: PartialFunction[SystemMessage, Unit] = {
      case _: SystemMessage.Start.Started => // it's ok
    }

    agents // init lazy
  }
}

