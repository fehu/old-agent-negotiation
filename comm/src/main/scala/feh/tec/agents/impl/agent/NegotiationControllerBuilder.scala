package feh.tec.agents.impl.agent

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout
import feh.tec.agents
import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.NegotiationController.ScopesInitialization
import feh.tec.agents._
import feh.tec.agents.impl.Agent.{AgentReporting, Id}
import feh.tec.agents.impl.NegotiationController.GenericStaticAgentsInit.Timings
import feh.tec.agents.impl.NegotiationController.{Counter, GenericStaticAgentsInit, GenericStaticInitArgs, Timeouts}
import feh.tec.agents.impl.agent.AgentCreation.NegotiationInit
import feh.tec.agents.impl.{ProposalEngine, NegotiationController, ReportRegisterImpl, System}
import feh.tec.agents.spec.NegotiationSpecification._
import feh.util._

import scala.concurrent.duration._
import scala.reflect.ClassTag

trait NegotiationControllerBuilder[Control <: NegotiationController with ScopesInitialization]
  extends agents.NegotiationControllerBuilder[spec.NegotiationSpecification, Control]
{

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
  def timeouts: Timeouts
  def timings: Timings

  def buildAgentsCount(defs: Seq[SpawnDef]) = defs.collect{ case SimpleSpawnDef(mp) => mp }.flatten.toMap

  protected def buildVars(defs: Seq[VarDef[_]]): Map[String, Var] = defs.map{
    case VarDef(name, GenericDomainDef(dom, clz)) =>
      val clazz = replaceClass(clz)
      name -> new Var(name, clazz.isInstance) {
        type Domain = Any
        def domain = dom
      }
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
            val constraints = extra.collect {
              case AgentConstraintsDef(constr) => constr
            }.flatten
            (negotiations(neg), (negId, competitors), negId -> AgentConstraintsDef(constraints))
        }.unzip3
        val constraints = _constraints.toMap
        val competitors = _competitors.toMap.mapValues{ case InterlocutorsByRoles(roles) => roles }
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
            conflictResolveTimeout = timeouts.resolveConflict,
            Role(role),
            domainIterators.asInstanceOf[Map[Var, DomainIterator[Var#Domain, Var#Tpe]]],
            constraints, // Map[NegotiationId, AgentConstraintsDef]
            reportingTo = args.reportingTo,
            checkConstraintsRepeat = timings.checkConstraintsRepeat
          )
      })
    }.toMap

  protected def defaultDomainIterator(v: Var): DomainIterator[_, _] = {
    val straightForward = v.domain match {
      case range: Range => new DomainIterator.Range()
      case iterable: Iterable[_] => new DomainIterator.Generic[Any]
    }
    DomainIterator.Random(straightForward)
  }

  protected def replaceClass(clazz: Class[_]) = clazz match{ // replace primitive type classes by the boxes
    case c if c == classOf[Int] => classOf[Integer]
    case c if c == classOf[Long] => classOf[java.lang.Long]
    case c if c == classOf[Byte] => classOf[java.lang.Byte]
    case c if c == classOf[Float] => classOf[java.lang.Float]
    case c if c == classOf[Double] => classOf[java.lang.Double]
    case c if c == classOf[Char] => classOf[Character]
    case c if c == classOf[Boolean] => classOf[java.lang.Boolean]
    case c => c
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

    override def negotiationIsFinished(neg: NegotiationId) = {
      super.negotiationIsFinished(neg)
      log.info("Negotiation Finished")
    }
  }


  class Default[AgImpl <: GenericIteratingAgentCreation[_]]
               (implicit acSys: ActorSystem, defaultAgentImplementation: ClassTag[AgImpl])
    extends NegotiationControllerBuilder[NegotiationController.GenericStaticAgentsInit[DefaultBuildAgentArgs]]
  {
    type BuildAgentArgs = DefaultBuildAgentArgs

    val defaultTimeouts = Timeouts(
      resolveConflict = 50 millis,
      agentCreation = 50 millis,
      agentStartup = 50 millis
    )

    val defaultTimings = Timings(
      retryToStartAgent = 50 millis,
      checkConstraintsRepeat = 200 millis,
      controlAcceptanceCheckDelay = 1 second
    )

    var vars: Map[String, Var] = null
    var negotiations: Map[String, (NegotiationId, (Set[Var], (Priority) => NegotiationInit))] = null
    var agentsCount: Map[String, Int] = null
    var agents: Map[String, (Map[NegotiationId, Set[String]], AgentBuilder[_ <: AbstractAgent, Args], BuildAgentArgs => Args) forSome { type Args <: Product }] = null
    var timeouts: Timeouts = null
    var timings: Timings = null

    def systemAgents: Map[(AgentBuilder[Ag, Arg], ClassTag[Ag]) forSome {type Ag <: AbstractAgent; type Arg <: Product}, Int] = Map(
      (AgentBuilder.SystemArgs0ServiceBuilder, scala.reflect.classTag[System.ConflictResolver]) -> 1,
      (AgentBuilder.SystemArgs2ServiceBuilder[FiniteDuration, ActorRef], scala.reflect.classTag[ReportRegisterImpl.Service]) -> 1
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

    def apply(v1: spec.NegotiationSpecification) = {
      vars = buildVars(v1.variables.map{ case vd: VarDef[_] => vd })
      negotiations = buildNegotiations(v1.negotiations)
      agents = buildAgents(v1.agents)
      agentsCount = Map("Queen" -> 4) //buildAgentsCount(v1.config.agentSpawn) todo
      timeouts = buildTimeouts(Map()) //v1.config.configs.collect { case TimeoutsDef(t) => t }.flatten.toMap |> buildTimeouts
      timings  = buildTimings(Map()) //v1.config.configs.collect { case TimingsDef(t)  => t }.flatten.toMap |> buildTimings

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
  extends PriorityBasedCreation[Lang](args.uuid, args.negotiationInit, args.conflictResolver, args.conflictResolveTimeout)
  with AgentReporting[Lang]
{
  self: ProposalEngine.Iterating[Lang] =>


  object DEBUG{
    var Constraint = false
  }

  def reportingTo = args.reportingTo
  def checkConstraintsRepeat = args.checkConstraintsRepeat

  override lazy val id = Id.withName(role, args.uuid, args.name)
  lazy val role: Role = args.role
  def domainIterators = args.domainIterators
  lazy val constraints = args.constraints.flatMap{
    case (negId, AgentConstraintsDef(cDefs)) =>
      cDefs map {
        case AgentConstraintDef(name, description, test) =>
          val dz = description.zipWithIndex.map(_.swap)
          val over = dz.collect{
            case (i, ConstraintParamDescription("proposed", varName, _)) =>
              i -> get(negId).currentValues.find(_._1.name == varName).get._1
          }
          val dependsOn = dz.collect{
            case (i, ConstraintParamDescription("valueOf", varName, _)) =>
              i -> get(negId).currentValues.find(_._1.name == varName).get._1
          }

          val f: (Map[Var, Any], Map[Var, Any]) => Boolean = (props, valsOf) => {
            val iProps = props.map{
              case (vr, vl) =>
                val Some((i, _)) = over.find(_._2 == vr)
                i -> vl
            }

            val iValsOf = valsOf.map{
              case (vr, vl) =>
                val Some((i, _)) = dependsOn.find(_._2 == vr)
                i -> vl
            }

            val args = (iProps ++ iValsOf).ensuring(_.size == dz.size, "Wrong number of args for constraint")
              .toSeq.sortBy(_._1).map(_._2)

            val arg = dz.size match {
              case 1 => Tuple1(args(0))
              case 2 => (args(0), args(1))
              case 3 => (args(0), args(1), args(2))
              case 4 => (args(0), args(1), args(2), args(3))
              case 5 => (args(0), args(1), args(2), args(3), args(4))
              case 6 => (args(0), args(1), args(2), args(3), args(4), args(5))
            }

            val res = test(arg)
            if(DEBUG.Constraint) log.info(s"Constraint($name) in $negId:\nover=$props\ndependsOn=$valsOf\narg=$arg\nres=$res")
            res
          }
          Constraint(name, negId, over.map(_._2).toSet, dependsOn.map(_._2).toSet, f)
      }


  }.toSet

}

object GenericIteratingAgentCreation{
  case class Args(uuid: UUID,
                  name: String,
                  negotiationInit: Map[NegotiationId, NegotiationInit],
                  conflictResolver: AgentRef,
                  conflictResolveTimeout: Timeout,
                  role: Role,
                  domainIterators: Map[Var, DomainIterator[Var#Domain, Var#Tpe]],
                  constraints: Map[NegotiationId, AgentConstraintsDef],
                  reportingTo: AgentRef,
                  checkConstraintsRepeat: FiniteDuration)

  object Builder extends AgentBuilder[GenericIteratingAgentCreation[_], Args]
}