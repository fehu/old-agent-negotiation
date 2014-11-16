package feh.tec.agents.light.impl

import akka.actor.{ActorLogging, ActorSystem, Actor, Props}
import akka.util.Timeout
import feh.tec.agents.light.AgentCreationInterface.NegotiationInit
import feh.tec.agents.light.impl.NegotiationEnvironmentController._
import feh.tec.agents.light._
import feh.tec.agents.light.spec.NegotiationSpecification.{AgentNegDef, AgentDef, InterlocutorsByRoles, Interlocutors}
import scala.concurrent.{Await, Future}
import akka.pattern.ask

import scala.reflect.ClassTag

trait NegotiationEnvironmentController extends EnvironmentController with DynamicEnvironmentController with ActorLogging{
//  controller =>

  type CreateInterface = Map[String, Any]
  implicit def asys: ActorSystem = context.system

//  protected val initialAgents: List[(AgentDef, () => AgentRef)]
  protected val spawns: Map[String, Int]
  protected val issues: Map[String, Var]
  protected val issuesByNegotiation: Map[String, Seq[String]]
  protected val initialAgents: List[(AgentDef, CreateInterface => AgentRef)]
  protected val timeouts: Timeouts
  protected val systemAgentsInit: Set[() => AgentRef]
  protected def extraArgs(agent: String): Map[String, Any]

  protected var state: NegotiationState = NegotiationState.Created
  protected var currentAgents: Set[AgentRef] = null
  protected var systemAgents: Set[AgentRef] = null

  protected lazy val initialAgentsByName = initialAgents.map(x => x._1.name -> x).toMap

  implicitly[ActorSystem].actorOf(Props.apply(new Actor{
    def receive: Actor.Receive = Map()
  }))

  def info =
    s""" NegotiationEnvironmentController:
       |   spawns = $spawns
       |   issues = $issues
       |   initialAgents = $initialAgents
       |   systemAgentsInit = $systemAgentsInit
       |   timeouts = $timeouts
       |   state = $state
       |   currentAgents = $currentAgents
       |   systemAgents = $systemAgents
     """.stripMargin

  println(info)

  var sysAgentByRole: Map[SystemRole, AgentRef] = null


  def createAgent(c: Int, d: AgentDef, f: CreateInterface => AgentRef, priority: Priority) = d match {
    case AgentDef(nme, rle, negDefs, _) =>
      val uniqueName = s"$nme-$c"
      val negInits = negDefs.toSet.map{
        {
          case AgentNegDef(negName, scope, extra) =>
            NegotiationInit(NegotiationId(negName), issuesByNegotiation(negName).map(issues).toSet)
        }: (AgentNegDef => NegotiationInit)}
      val args = extraArgs(nme)

      log.debug("negInits = " + negInits)
      f(Map(
        "uniqueName" -> uniqueName,
        "role" -> NegotiationRole(rle),
        "negotiationsInit" -> negInits,
        "args" -> args
      )) -> d
    }

  def initialize(): Unit =
    if (state == NegotiationState.Created){
      log.info("initializing")
      state = NegotiationState.Initializing
      systemAgents = systemAgentsInit.map(_())
      sysAgentByRole = systemAgents.map(ag => ag.id.role.asInstanceOf[SystemRole] -> ag).toMap

      log.debug("initializing")
      val refsAndDefs = spawns flatMap {
        case (agName, count) =>
          val (agDef, f) = initialAgentsByName(agName)
          log.debug(s"agName = $agName, count = $count, agDef = $agDef, f = $f")
          for (c <- 1 to count)
            yield createAgent(c, agDef, f, new Priority(c))
      }
      currentAgents = refsAndDefs.keySet

      log.debug("currentAgents = " + currentAgents)
      initialAgentsCreated(refsAndDefs.mapValues{
        adef => adef.negotiations.map{
          case AgentNegDef(neg, scope, _) => NegotiationId(neg) -> scope
        }.toMap
      })
      currentAgents foreach sendAndProcessAnswer(SystemMessage.Initialize, timeouts.initialize, _.mapTo[SystemMessage.Initialized.type])
      log.debug("sendAndProcessAnswer")
      state = NegotiationState.Initialized
    }
    else wrongMessageForCurrentState(SystemMessage.Initialize)

  def start(): Unit =
    if (state == NegotiationState.Initialized || state == NegotiationState.Stopped) {
      state = NegotiationState.Starting
      currentAgents foreach sendAndProcessAnswer(SystemMessage.Start, timeouts.start, _.mapTo[SystemMessage.Started.type])
      state = NegotiationState.Negotiating
    }
    else wrongMessageForCurrentState(SystemMessage.Start)

  def stop(): Unit =
    if(state == NegotiationState.Negotiating){
      currentAgents foreach sendAndProcessAnswer(SystemMessage.Stop, timeouts.stop, _.mapTo[SystemMessage.Stopped.type])
      state = NegotiationState.Stopped
    }
    else wrongMessageForCurrentState(SystemMessage.Stop)

  def reset(): Unit =
    if(state == NegotiationState.Stopped){
      currentAgents foreach sendAndProcessAnswer(SystemMessage.Reset, timeouts.reset, _.mapTo[SystemMessage.Reset.type])
      state = NegotiationState.Initialized
    }
    else wrongMessageForCurrentState(SystemMessage.Reset)

  protected def sendAndProcessAnswer[R](msg: AbstractMessage, timeout: Timeout, f: Future[Any] => Future[R])(ag: AgentRef): R = {
    log.debug(s"sendAndProcessAnswer(msg = $msg, timeout = $timeout)")
    implicit def t = timeout
    val res = Await.result(f(ag.ref ? msg), t.duration)
    log.debug(s"sendAndProcessAnswer(res = $res)")
    res
  }

  protected def newScope(ag: AgentRef, scope: Set[AgentRef], neg: NegotiationId) =
    ag.ref ! SystemMessage.ScopeUpdate.NewScope(scope, neg)

  def initialAgentsCreated(ag: Map[AgentRef, Map[NegotiationId, Interlocutors]]): Unit = ag foreach{
    case (ref, negotiations) => negotiations foreach{
      case (negId, InterlocutorsByRoles(roles)) =>
        val scope = currentAgents.filter(roles contains _.id.role.name).filter(_.id != ref.id)
        newScope(ref, scope, negId)
    }
  }

  private def wrongMessageForCurrentState(msg: SystemMessage) = sender() ! SystemMessage.UnexpectedCommand(msg, state)

  /** override me */
  def agentAdded(ag: AgentRef, neg: Set[NegotiationId]): Unit = ???
  /** override me */
  def agentRemoved(ag: AgentRef, neg: Set[NegotiationId]): Unit = ???

  def receive: Receive = {
    case SystemMessage.Initialize => initialize()
    case SystemMessage.Start      => start()
    case SystemMessage.Stop       => stop()
    case SystemMessage.Reset      => reset()
  }

  val name = NegotiationEnvironmentController.Name
  val role = NegotiationEnvironmentController.Role
}

object NegotiationEnvironmentController{
  trait Timeouts{
    def initialize: Timeout
    def start: Timeout
    def stop: Timeout
    def reset: Timeout
  }

  lazy val Name = "NegotiationEnvironmentController"
  lazy val Role = SystemRole(Name)

  object DefaultTimeouts extends Timeouts {
    import scala.concurrent.duration._

    def initialize = Timeout(100 millis)
    def start = Timeout(100 millis)
    def stop = Timeout(50 millis)
    def reset = Timeout(50 millis)
  }
}