package feh.tec.agents.impl

import java.util.UUID

import akka.actor.{Props, ActorSystem}
import akka.util.Timeout
import feh.tec.agents
import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.NegotiationController.ScopesInitialization
import feh.tec.agents._
import feh.tec.agents.impl.Agent.{AgentReporting, Id}
import feh.tec.agents.impl.NegotiationController.GenericStaticAgentsInit.Timings
import feh.tec.agents.impl.NegotiationController.{Timeouts, GenericStaticInitArgs, GenericStaticAgentsInit, Counter}
import feh.tec.agents.impl.NegotiationSpecification.ConstraintBuilder.{MustNot, Equal, Must}
import feh.tec.agents.impl.NegotiationSpecification.{TimeoutsDef, TimingsDef}
import feh.tec.agents.impl.agent.AgentBuilder
import feh.tec.agents.impl.agent.AgentCreation.NegotiationInit
import feh.tec.agents.impl.view.CreateConstraintsHelper
import scala.concurrent.duration._
import feh.util._

import scala.reflect.ClassTag

trait NegotiationControllerBuilder[Control <: NegotiationController with ScopesInitialization]
  extends agents.NegotiationControllerBuilder[NegotiationSpecification, Control]
{
  import NegotiationSpecification._

  type BuildAgentArgs <: Product{
    def priorityByNeg: NegotiationId => Int
    def conflictResolver: AgentRef
    def reportingTo: AgentRef
    def countByRole: String => Int
  }

  def vars: Map[String, Var]
  def negotiations: Map[String, (NegotiationId, (Set[Var], Priority => NegotiationInit))]
  def agents: Map[String, (Map[NegotiationId, Set[String]], AgentBuilder[_ <: AbstractAgent, Args], BuildAgentArgs => Args) forSome { type Args <: Product }]
  def agentsCount: Map[String, Int]

  def buildAgentsCount(defs: Seq[SpawnDef]) = defs.collect{ case SimpleSpawnDef(mp) => mp }.flatten.toMap

  protected def buildVars(defs: Seq[VarDef]): Map[String, Var] = defs.map{
    case VarDef(name, domainDef) =>
      name -> (domainDef match {
        case dom@DomainRange(range) => new Var(name, dom.clazz.isInstance) with Domain.Range{ def domain = range }
        case dom@DomainSet(set: Set[Any]) => new Var(name, dom.clazz.isInstance) with Domain.Small[Any]{ def domain = set }
      })
  }.toMap

  protected def buildNegotiations(defs: Seq[NegotiationDef]): Map[String, (NegotiationId, (Set[Var], Priority => NegotiationInit))] =
    defs.map{
      case NegotiationDef(name, issues) => 
        val id = NegotiationId(name)
        name -> (
          id -> (issues.toSet.map(vars) -> ((priority: Priority) => NegotiationInit(priority, issues.toSet.map(vars))))
      )
    }.toMap
  
  protected def buildAgents(defs: Seq[AgentDef]): Map[String, (Map[NegotiationId, Set[String]], AgentBuilder[_, Args], BuildAgentArgs => Args) forSome { type Args <: Product }] =
    defs.map{
      case AgentDef(name, role, negs) =>
        val (_negExtracted, _competitors, _constraints) = negs.map{
          case AgentNegDef(neg, competitors, extra) =>
            val (negId, negCreate) = negotiations(neg)
            val constraints = extra.collect{
              case AgentConstraintsDef(c) => c.map{
                case AgentConstraintDef(issue, Must(Equal)) =>
                  (ag: CreateConstraintsHelper) => ag.CreateConstraint.equals(vars(issue), negId)
                case AgentConstraintDef(issue, MustNot(Equal)) =>
                  (ag: CreateConstraintsHelper) => ag.CreateConstraint.notEquals(vars(issue), negId)
              }
            }.flatten
            (negotiations(neg), (negId, competitors), constraints)
        }.unzip3
        val constraints = _constraints.flatten
        val competitors = _competitors.toMap
        val negExtracted = _negExtracted.toMap

        val domainIterators = negExtracted.flatMap(_._2._1).toSeq.zipMap(defaultDomainIterator).toMap

        name -> (competitors, GenericIteratingAgentCreation.Builder, {
        (args: BuildAgentArgs) =>
          GenericIteratingAgentCreation.Args(
            UUID.randomUUID(),
            name + "-" + args.countByRole(role),
            negotiationInit = negExtracted.map{
              case (negId, (vars, negInit)) => negId -> negInit(new Priority(args.priorityByNeg(negId)))
            },
            args.conflictResolver,
            conflictResolveTimeout = Timeout(30 millis), // todo
            Role(role),
            domainIterators.asInstanceOf[Map[Var, DomainIterator[Var#Domain, Var#Tpe]]],
            constraints,
            reportingTo = args.reportingTo
          )
      })
    }.toMap

  protected def defaultDomainIterator(v: Var): DomainIterator[_, _] = v match {
    case range: Domain.Range => new DomainIterator.Range()
    case range: Domain.Small[_] => new DomainIterator.Generic[Any]
  }
}

object NegotiationControllerBuilder{

  case class DefaultBuildAgentArgs(priorityByNeg: NegotiationId => Int,
                                   conflictResolver: AgentRef,
                                   reportingTo: AgentRef,
                                   countByRole: String => Int)



  class DefaultController(arg: GenericStaticInitArgs[DefaultBuildAgentArgs])
    extends NegotiationController.GenericStaticAgentsInit[DefaultBuildAgentArgs](arg)
  {
    lazy val roleCounter = new Counter[String, Int](0, 1+)

    def startingPriority = new Priority(0)

    protected def buildAgentArgs = DefaultBuildAgentArgs(
      priorityByNeg = negId => assigningPriority.next(negId),
      conflictResolver,
      reportingTo,
      countByRole = roleCounter.next
    )
  }


  class Default[AgImpl <: GenericIteratingAgentCreation[_]]
               (implicit acSys: ActorSystem, defaultAgentImplementation: ClassTag[AgImpl])
    extends NegotiationControllerBuilder[NegotiationController.GenericStaticAgentsInit[DefaultBuildAgentArgs]]
  {
    type BuildAgentArgs = DefaultBuildAgentArgs

    val defaultTimeouts = Timeouts(
      resolveConflict = 30 millis,
      agentCreation = 10 millis,
      agentStartup = 30 millis
    )

    val defaultTimings = Timings(
      retryToStartAgent = 50 millis
    )

    var vars: Map[String, Var] = null
    var negotiations: Map[String, (NegotiationId, (Set[Var], (Priority) => NegotiationInit))] = null
    var agentsCount: Map[String, Int] = null
    var agents: Map[String, (Map[NegotiationId, Set[String]], AgentBuilder[_ <: AbstractAgent, Args], BuildAgentArgs => Args) forSome { type Args <: Product }] = null
    var timeouts: Timeouts = null
    var timings: Timings = null

    def systemAgents: Map[(AgentBuilder[Ag, Arg], ClassTag[Ag]) forSome {type Ag <: AbstractAgent; type Arg <: Product}, Int] = Map(
      (AgentBuilder.SystemArgs0Service, scala.reflect.classTag[System.ConflictResolver]) -> 1,
      (AgentBuilder.SystemArgs0Service, scala.reflect.classTag[ReportsPrinter]) -> 1
    )

    private def agentsToAgentInits = agents.toSeq map{
      case (k, Tuple3(negRoles, builder, buildArgs)) =>
        GenericStaticAgentsInit.AgentInit[BuildAgentArgs, Product, AbstractAgent](
          defaultAgentImplementation,
          builder.asInstanceOf[AgentBuilder[AbstractAgent, Product]],
          buildArgs,
          actorName = k,
          scopes = negRoles,
          agentsCount(k)
        )
    }
    
    def buildTimeouts(timeouts: Map[String, FiniteDuration]) = (defaultTimeouts /: timeouts.mapKeys(_.toLowerCase)){
      case (acc, ("creation", time)) => acc.copy(agentCreation = time)
      case (acc, ("startup", time)) => acc.copy(agentStartup = time)
      case (acc, ("resolve conflict", time)) => acc.copy(resolveConflict = time)
    }
    
    def buildTimings(timings: Map[String, FiniteDuration]) = (defaultTimings /: timings.mapKeys(_.toLowerCase)){
      case (acc, ("retry startup", time)) => acc.copy(retryToStartAgent = time)
    }

    def apply(v1: NegotiationSpecification) = {
      vars = buildVars(v1.variables)
      negotiations = buildNegotiations(v1.negotiations)
      agents = buildAgents(v1.agents)
      agentsCount = buildAgentsCount(v1.config.agentSpawn)
      timeouts = v1.config.configs.collect { case TimeoutsDef(t) => t }.flatten.toMap |> buildTimeouts
      timings  = v1.config.configs.collect { case TimingsDef(t)  => t }.flatten.toMap |> buildTimings

      val arg = GenericStaticInitArgs[DefaultBuildAgentArgs](
        systemAgentBuilders = systemAgents,
        negotiationIds = negotiations.map(_._2._1).toSet,
        agentBuilders = agentsToAgentInits,
        timeouts,
        timings
      )

      acSys.actorOf(props(arg), "NegotiationController")
    }

    protected def props(arg: GenericStaticInitArgs[DefaultBuildAgentArgs]) = Props(classOf[DefaultController], arg)

  }
}

abstract class GenericIteratingAgentCreation[Lang <: ProposalLanguage](args: GenericIteratingAgentCreation.Args)
  extends agent.PriorityBasedCreation[Lang](args.uuid, args.negotiationInit, args.conflictResolver, args.conflictResolveTimeout)
  with CreateConstraintsHelper with AgentReporting[Lang]
{
  self: ProposalEngine.Iterating[Lang] =>


  def reportingTo = args.reportingTo

  override lazy val id = Id.withName(role, args.uuid, args.name)
  lazy val role: Role = args.role
  def domainIterators = args.domainIterators
  lazy val constraints = args.constraints.map(_(this)).toSet

}

object GenericIteratingAgentCreation{
  case class Args(uuid: UUID,
                  name: String,
                  negotiationInit: Map[NegotiationId, NegotiationInit],
                  conflictResolver: AgentRef,
                  conflictResolveTimeout: Timeout,
                  role: Role,
                  domainIterators: Map[Var, DomainIterator[Var#Domain, Var#Tpe]],
                  constraints: Seq[CreateConstraintsHelper => Constraint[Var]],
                  reportingTo: AgentRef)

  object Builder extends AgentBuilder[GenericIteratingAgentCreation[_], Args]
}