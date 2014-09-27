package feh.tec.agents.impl

import akka.actor.ActorLogging
import akka.pattern.ask
import akka.util.Timeout
import feh.tec.agents.Message.AutoId
import feh.tec.agents.NegotiationController.ScopesInitialization
import feh.tec.agents.SystemMessage.ScopeUpdate
import feh.tec.agents._
import feh.tec.agents.impl.NegotiationController.GenericStaticAgentsInit.Timings
import feh.tec.agents.impl.System.Service
import feh.tec.agents.impl.agent.AgentBuilder.{SystemArgs0ServiceBuilder, SystemArgs2ServiceBuilder}
import feh.tec.agents.impl.agent.{AgentBuilder, Tuple0}
import feh.tec.agents.service.ConflictResolver
import feh.util._

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

object NegotiationController{
  object Request{
    case class AgentRefs() extends SystemMessage with AutoId
  }

  case object Role extends SystemRole{ val name = "Negotiation Controller" }

  case class Timeouts(resolveConflict: Timeout, agentCreation: Timeout, agentStartup: Timeout)

  
  
  /** System Agent Service Builder */
  trait SystemServiceBuilding{
    sysAgent: SystemAgent =>

    implicit def acSys = context.system

    protected def systemAgentBuilders: Map[(AgentBuilder[Ag, Arg], ClassTag[Ag]) forSome {type Ag <: AbstractAgent; type Arg <: Product}, Int]
    def timeouts: Timeouts
    def timings: Timings

    lazy val systemAgents = {
      systemAgentBuilders.toSeq.par.flatMap{
        case (Tuple2(b: SystemArgs0ServiceBuilder, cTag), count) =>
          for(_ <- 1 to count) yield b.create(Tuple0, extracting = true)(cTag, implicitly, timeouts.agentCreation)
        case (Tuple2(b: SystemArgs2ServiceBuilder[Any, Any], cTag), count) =>
          val build = systemArgs2Service(b -> cTag)
          for(_ <- 1 to count) yield build()
      }.toList
    }

    protected def systemArgs2Service: PartialFunction[(SystemArgs2ServiceBuilder[Any, Any], ClassTag[_ <:AbstractAgent]), () => AgentRef] = {
      case (b, cTag) if cTag.runtimeClass == classOf[ReportRegisterImpl] => reportRegisterImpl(b, cTag)
    }

    private def reportRegisterImpl(b: SystemArgs2ServiceBuilder[Any, Any], cTag: ClassTag[_ <:AbstractAgent]) = {
      val builder = b.asInstanceOf[SystemArgs2ServiceBuilder[FiniteDuration, AgentRef]]
      val tag = cTag.asInstanceOf[ClassTag[Service.Args2[FiniteDuration, AgentRef] with AbstractAgent]]
      val args = timings.controlAcceptanceCheckDelay -> ref
      () => builder.create(args, extracting = true)(tag, implicitly, timeouts.agentCreation)
    }
  }
  
  trait NegotiationControllerBase extends NegotiationController with SystemAgent with SystemServiceBuilding with ActorLogging{
    def role = Role

    implicit def exContext = context.dispatcher

    def handleStartup: PartialFunction[SystemMessage, Unit]

    def conflictResolver = getSystemAgent(ConflictResolver.Role)
    def reportingTo = getSystemAgent(ReportArchive)
    def knowledgeShare = getSystemAgent(KnowledgeSharing.Role)

    def restart() = {
      negotiationFinished.clear()
    }

    def start() = {
      implicit def timeout = timeouts.agentStartup

      if(negotiationFinished.nonEmpty) restart()
      systemAgents map (_.ref ! SystemMessage.Start())
      agents map (_.ref ? SystemMessage.Start() |> (_.mapTo[SystemMessage] map handleStartup))
    }

    def stop() = {
      implicit def timeout = timeouts.agentStartup

      agents map (_.ref ? SystemMessage.Stop() |> (_.mapTo[SystemMessage] map {
        case SystemMessage.Stopped(_) => // it's ok
      }))
    }

    def resume() = {
      implicit def timeout = timeouts.agentStartup

      agents map (_.ref ? SystemMessage.Resume() |> (_.mapTo[SystemMessage] map {
        case SystemMessage.Resumed(_) => // it's ok
      }))
    }

    protected lazy val negotiationFinished = mutable.HashMap.empty[NegotiationId, Boolean].withDefaultValue(false)
    
    def negotiationIsFinished(neg: NegotiationId) = stop()

    // todo: stop
    override def processSys: PartialFunction[SystemMessage, Unit] = super.processSys orElse{
      case req@Request.AgentRefs()            => sender() ! agents
      case SystemMessage.Start()              => start()
      case SystemMessage.Resume()             => resume()
      case SystemMessage.Stop()               => stop()
      case SystemMessage.NegotiationFinished(neg) if !negotiationFinished(neg) =>
        negotiationFinished += neg -> true
        negotiationIsFinished(neg)
      case _: SystemMessage.NegotiationFinished =>
    }

    protected def getSystemAgent(role: Role) = systemAgents.filter(_.id.role == role).ensuring(_.size == 1).head
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
    self: NegotiationControllerBase =>

    def startingPriority: Priority

    lazy val assigningPriority = new Counter[NegotiationId, Priority](startingPriority, _.raise())
  }


  object GenericStaticAgentsInit{
    case class AgentInit[BuildAgentArgs, Args <: Product, Ag <: AbstractAgent](
                                               agTag: ClassTag[_ <: Ag],
                                               builder: AgentBuilder[Ag, Args],
                                               buildArgs: BuildAgentArgs => Args,
                                               actorName: String,
                                               scopes: Map[NegotiationId, Set[String]],
                                               count: Int)
    case class Timings(retryToStartAgent: FiniteDuration,
                       checkConstraintsRepeat: FiniteDuration,
                       controlAcceptanceCheckDelay: FiniteDuration)
  }

  case class GenericStaticInitArgs[BuildAgentArgs](
              systemAgentBuilders: Map[(AgentBuilder[Ag, Arg], ClassTag[Ag]) forSome {type Ag <: AbstractAgent; type Arg <: Product}, Int],
              negotiationIds: Set[NegotiationId],
              agentBuilders: Seq[GenericStaticAgentsInit.AgentInit[BuildAgentArgs, _, _]],
              timeouts: Timeouts,
              timings: GenericStaticAgentsInit.Timings
                                                    )

  abstract class GenericStaticAgentsInit[BuildAgentArgs](arg: GenericStaticInitArgs[BuildAgentArgs])
    extends NegotiationControllerBase with ScopesInitialization with PriorityAssignation with ActorLogging
  {
    import feh.tec.agents.impl.NegotiationController.GenericStaticAgentsInit._

    protected def buildAgentArgs: BuildAgentArgs

    def systemAgentBuilders = arg.systemAgentBuilders
    def timeouts = arg.timeouts
    def timings = arg.timings

    lazy val agentsInfo = arg.agentBuilders.flatMap{
      case init@AgentInit(cTag, builder, bArgs, actorName, scopes, count) =>
        for(i <- 1 to count) yield
          builder.create(bArgs(buildAgentArgs), actorName + "-" + i)(cTag, implicitly, timeouts.agentCreation) -> (scopes, count)
    }.toMap

    lazy val agents = agentsInfo.keys.toSeq

    def scopeFor(ag: AgentRef, in: NegotiationId): Set[AgentRef] = {
      agentsInfo(ag)._1(in).flatMap{
        case role => agents.filter(a => a.id.role.name == role && a != ag)
      }
    }

    def handleStartup: PartialFunction[SystemMessage, Unit] = {
      case msg: SystemMessage.Start.Started => log.info(msg.id + " successfully started")
    }

    override def start(): Unit = {
      systemAgents; agents // init lazy
      knowledgeShare.ref ! ScopeUpdate.NewScope(agents.toSet, null)
      arg.negotiationIds foreach updateScopes
      super.start()
    }

  }
}

