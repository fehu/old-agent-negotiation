package feh.tec.agents.comm.coloring

import feh.tec.agent.comm._
import java.awt.Color
import java.util.UUID
import feh.util._
import scala.collection.mutable
import feh.tec.agent.comm.Agent.{Response, CommAgentController}
import akka.actor._
import akka.util.Timeout
import akka.pattern._
import scala.concurrent.{Await, Future, ExecutionContext}
import ColoringGraph._
import feh.tec.agents.comm.coloring.ColoringOverseer._
import feh.util.{Undirected, Graph}
import scala.util.Random
import feh.tec.agents.coloring.util._
import feh.util.Graph.{GraphRef, Node}
import feh.tec.agents.comm.coloring.ColoringExpression.Accept
import feh.tec.agents.comm.coloring.ColoringExpression.Reject
import feh.tec.agent.comm.Agent.Communicating.{ResponseDelay, AgentActor}
import feh.tec.agents.comm.coloring.ColoringAgent.ColoringActorAgent
import feh.tec.agents.comm.coloring.ColoringAgentsImpl.DelayedActorAgentImpl
import feh.tec.agents.comm.coloring.ColoringGraphGenerator.RandConfig
import scala.concurrent.duration._

class ColoringEnvironment(val colors: Set[Color], val graph: ColoringGraph) {
  env =>

  protected val _nodes = mutable.HashMap(graph.nodes.toSeq.map(tuple(_.id, identity)): _*)

  def nodes = _nodes.toMap

  def setColor(id: UUID, color: Option[Color]) = _nodes += id -> _nodes(id).newValue(color)
}

object ColoringOverseer{
  case class SetColor(node: UUID, color: Option[Color])
  case class GetColor(node: UUID)
  case object GetColors
  case class GetNNeighbours(of: UUID)
  case class NNeighbours(of: UUID, n: Int)
  case class Working(node: UUID)
  case class Done(node: UUID)

  class OverseerActor(env: ColoringEnvironment,
                      ensureDone: FiniteDuration,
                      done: () => Unit,
                      implicit val system: ActorSystem) extends Actor{
    import system.dispatcher
    val ready = mutable.HashMap(env.nodes.mapValues(_ => false).toSeq: _*)

    private def isDone = ready.forall(_._2)
    def ifDone(f: => Unit) = Future(isDone).filter(identity).foreach(_ => f)
    
    case object EnsuringDone
    var ensuringDoneInProgress = false
    
    def receive: Actor.Receive = {
      case GetColor(nodeId) => sender ! env.nodes(nodeId).color
      case SetColor(nodeId, color) => env.setColor(nodeId, color)
      case GetColors => sender ! env.nodes
      case GetNNeighbours(id) => env.graph.byId(id).foreach(n => sender ! NNeighbours(n.id, n.neighbours.size))
      case Working(nodeId) => ready += nodeId -> false
      case Done(nodeId) =>
        ready += nodeId -> true
        if(!ensuringDoneInProgress) ifDone{
          ensuringDoneInProgress = true
          system.scheduler.scheduleOnce(ensureDone, self, EnsuringDone)
        }
      case EnsuringDone =>
        ensuringDoneInProgress = false
        ifDone{ Future(done()) }
    }
  }
}

class ColoringOverseer(val env: ColoringEnvironment,
                       ensureDone: FiniteDuration,
                       done: () => Unit)
                      (implicit system: ActorSystem, timeout: Timeout){
  protected def props = Props(classOf[ColoringOverseer.OverseerActor], env, ensureDone, done, system)
  lazy val actor = system.actorOf(props, "ColoringOverseer")
  import system.dispatcher

  def validate = (actor ? GetColors).mapTo[Map[UUID, ColoringGraph.Node]].map{
    nodes =>
      env.graph.edges.filter{
        case (i1, i2) =>
          val c1 = nodes(i1).color
          c1.isDefined && c1 == nodes(i2).color
      }
  }
}

object ColoringEnvironmentRef{
  def apply(env: ColoringEnvironment, overseerRef: ActorRef, naming: Map[UUID, Name], timeout: Timeout)(node: UUID) =
    new ColoringEnvironmentRef(node, env.colors, overseerRef, naming, timeout)
}

class ColoringEnvironmentRef(val nodeId: UUID, 
                             val possibleColors: Set[Color],
                             protected val overseerRef: ActorRef, 
                             val naming: Map[UUID, Name],
                             implicit val timeout: Timeout)
  extends EnvironmentRef[ColoringEnvironment]
{
  def setColor(color: Option[Color])(implicit context: ExecutionContext) = overseerRef ? SetColor(nodeId, color) map(_ => {})
  def color(implicit context: ExecutionContext) = (overseerRef ? GetColor(nodeId)).mapTo[Option[Color]]
  def nConnections(nodeId: UUID)(implicit context: ExecutionContext): Future[Int] = overseerRef.ask(GetNNeighbours(nodeId))
    .mapTo[NNeighbours].map(_.ensuring(_.of == nodeId).n)
  
  def notifyActiveState() = overseerRef ! Working(nodeId)
  def notifyInactiveState() = overseerRef ! Done(nodeId)
  
  lazy val reverseNaming = naming.map(_.swap).toMap 
}

object ColoringGraph{
  type Node = Graph.Node[Option[Color]]
  implicit class NodeColorWrapper(n: Node){
    def color = n.value
  }
}

case class ColoringGraph(override val ref: GraphRef, override val nodes: Set[ColoringGraph.Node], override val edges: Set[(UUID, UUID)])
  extends Graph[Option[Color]](null, null, null) with Undirected[Option[Color]]

class ColoringLanguage extends NegotiatingLanguage{
  type Expr = ColoringExpression
  type Issue = ColoringIssue
  type Accepted = Accept
  type Rejected = Reject
}

trait ColoringExpression{
  def sender: ActorRef
  def asText: String

  override def toString = s"[$sender] $asText"
}
trait ColoringIssue extends ColoringExpression

object ColoringExpression{
  case class Proposal(color: Color)(implicit val sender: ActorRef) extends ColoringIssue{
    def asText = s"I propose I set color $color"
  }
  case class Reject(offer: Color)(implicit val sender: ActorRef) extends ColoringIssue{
    def asText = s"Your offer ($offer) is unacceptable, i have already set it"
  }
  case class Accept(color: Color)(implicit val sender: ActorRef) extends ColoringExpression{
    def asText = s"I'm ok with your color assignation ($color)"
  }
  case class Confirm(color: Color)(implicit val sender: ActorRef) extends ColoringExpression{
    def asText = s"I confirm my color assignation ($color)"
  }
  case class Fallback()(implicit val sender: ActorRef) extends ColoringExpression{
    def asText = "I have no options to assign"
  }
  case class NotifyFallback()(implicit val sender: ActorRef) extends ColoringExpression{
    def asText = "I've done fallback"
  }
}

import ColoringExpression._

class ColoringAgentController(val gr: ColoringGraph, naming: UUID => ColoringAgent#Id)
  extends CommAgentController[ColoringLanguage, ColoringEnvironment, Name]
{
  override def tellAll(msg: ColoringLanguage#Expr)(implicit sender: ActorRef): Unit ={
    val neigh = gr.neighbouringNodes(agents(id(sender)).asInstanceOf[ColoringAgent].envRef.nodeId)
      .map(node => naming(node.id))
    neigh foreach (tell(_, msg))
  }
}

object ColoringAgent{
  protected case object Start
  //  protected case object Stop

  def start(agent: ActorRef) = agent ! Start

  abstract class ColoringActorAgent(name: Name,
                                    controller: CommAgentController[ColoringLanguage, ColoringEnvironment, Name],
                                    envRef: ColoringEnvironmentRef,
                                    val scheduler: Scheduler)
    extends AgentActor[Name, ColoringLanguage, ColoringEnvironment](name, controller, envRef) with AgentReport
  {
    implicit def execContext = context.dispatcher

    protected var myColor: Option[Color] = None

    def proposal: Proposal

    def onReceive(msg: ColoringLanguage#Expr) = report(msg.toString)

    def setColor(c: Option[Color]){
      myColor = c
      envRef.setColor(c).foreach{ _ =>
        report(s"Set color of my node to $c / ${c.map(_.hexRGB).getOrElse("")}")
      }
    }
    def isLangExpr(a: Any) = a.isInstanceOf[ColoringLanguage#Expr]

    case object SendProposals

    protected def stateActivate(){ envRef.notifyActiveState() }
    protected def stateDeactivate(){ envRef.notifyInactiveState() }

    object state{
      private var _isActive = false
      def isActive = _isActive
      def activate() = {
        _isActive = true
        stateActivate()
      }
      def deactivate()= {
        _isActive = false
        stateDeactivate()
      }
    }


    def onStart() {}

    override def receive = super.receive orElse {
      case Start =>
        report("I've started")
        self ! SendProposals
        onStart()
      case SendProposals =>
        state.activate()
        report("Sending proposals")
        controller.tellAll(proposal)
    }
  }

}

object AgentReport{
  object default{
    var enabled = false 
  }
}
trait AgentReport {
  self: AgentActor[_, _, _] =>

  object report{
    var enabled = AgentReport.default.enabled
  }
  
  def report(msg: String): Unit = if(report.enabled) alwaysReport(msg)
  def alwaysReport(msg: String) = println(s"[$id] " + msg.replaceAllLiterally("\n", "\n" + " " * (id.toString.length + 3)))
}

abstract class ColoringAgent(val name: Name,
                             val neighbours: Set[Name],
                             val envRef: ColoringEnvironmentRef,
                             val controller: ColoringAgentController)
                            (implicit protected val system: ActorSystem)
  extends Agent.Communicating[ColoringLanguage, ColoringEnvironment]
{
  type EnvRef = ColoringEnvironmentRef

  type Id = Name
  val id = name
  
  controller.register(this)

  def actorProps: Props
  lazy val actor = system.actorOf(actorProps, name)
}

object ColoringAgentsImpl{
  protected def response = Response.build[ColoringLanguage, ColoringEnvironment, Name]

  class DelayedActorAgentImpl(name: Name,
                                controller: CommAgentController[ColoringLanguage, ColoringEnvironment, Name],
                                envRef: ColoringEnvironmentRef,
                                scheduler: Scheduler,
                                neighbours: Set[Name],
                                proposalTickDelay: FiniteDuration,
                                val messageDelay: FiniteDuration)
    extends ColoringActorAgentImpl(name, controller, envRef, scheduler, proposalTickDelay, neighbours)
    with ResponseDelay[Name, ColoringLanguage, ColoringEnvironment]

  class ColoringActorAgentImpl(name: Name,
                                 controller: CommAgentController[ColoringLanguage, ColoringEnvironment, Name],
                                 envRef: ColoringEnvironmentRef,
                                 scheduler: Scheduler,
                                 proposalTickDelay: FiniteDuration,
                                 val neighbours: Set[Name])
    extends ColoringActorAgent(name, controller, envRef, scheduler)
  {
    def initTries = envRef.possibleColors.toSeq.randomOrder()
    var tries = initTries
    lazy val nNeighbours = neighbours.size 
    lazy val nNeighboursConnections = neighbours.zipMap(envRef.reverseNaming).toMap.par.mapValues{
      id => Await.result(envRef.nConnections(id), 200 millis)
    }.toMap
    
    var currentProposal = tries.head
    def nextProposal() = currentProposal = {
      tries match{
        case Nil =>
          tries = initTries
        case _ =>
      }
      val r = tries.head
      tries = tries.tail
      r
    }
    def proposal = Proposal(Option(currentProposal) orElse myColor getOrElse {
      nextProposal()
      currentProposal
    })

    def createAcceptanceMap = mutable.Map(neighbours.toSeq.zipMap(_ => false): _*)
    var acceptanceMap = createAcceptanceMap
    def allAccepted_? = acceptanceMap forall (_._2)

    val neighbourColorMap = mutable.Map(neighbours.toSeq.zipMap(_ => Option.empty[Color]): _*)

    def colorOccupied(by: Name, c: Color)  {
      neighbourColorMap += by -> Some(c)
    }
    def colorFreed(by: Name) {
      neighbourColorMap += by -> None
      acceptanceMap += by -> false
    }
    // assuming no myColor is set
    def calcFreeColors = envRef.possibleColors &~ neighbourColorMap.values.flatten.toSet

    def fallback() = {
      setColor(None)
      nextProposal()
      acceptanceMap = createAcceptanceMap
      self ! SendProposals
      response(tellAll = NotifyFallback())
    }

    def accepted(by: Name) = {
      acceptanceMap += by -> true
      if(allAccepted_?) {
        // ensure no one has set it
        val contradicting = neighbourColorMap.flatMap(p => p._2.find(currentProposal ==).map(p._1 ->))
        if(contradicting.isEmpty) colorAccepted()
        else response.seq(personal = contradicting.mapValues(_ => proposal).toList)
      }
      else response()
    }

    def colorAccepted() = {
      setColor(Some(currentProposal.ensuring(_ != null)))
      tries = tries.filterNot(currentProposal ==)
      response(tellAll = Confirm(currentProposal)) $$ {
        currentProposal = null
        state.deactivate()
      }
    }

    def ignoreFallbackRequestCoeff = .4
    def fallbackDespiteCoeff = .4
    // p diminishes as tries's size diminishes
    def triesDependantProbDim = (_: Double) * tries.size.toDouble / envRef.possibleColors.size > Random.nextDouble()
    // p rises as tries's size diminishes
    def triesDependantProbCres = (_: Double) * (1 - tries.size.toDouble / envRef.possibleColors.size) > Random.nextDouble()
    def ignoreFallbackRequest = triesDependantProbCres(ignoreFallbackRequestCoeff)
    def fallbackDespiteNNeighbours = triesDependantProbDim(fallbackDespiteCoeff)

    def ifHasNotLessNeighbors[R](sender: Name)(f: => Response[ColoringLanguage, ColoringEnvironment, Name]) =
      nNeighboursConnections.get(sender).map{
        n => 
          if(n >= nNeighbours && !ignoreFallbackRequest || fallbackDespiteNNeighbours) Some(f)
          else None
      }.getOrElse{
        sys.error(s"$sender is not neighbour of $name")
      }
    
    def respond = {
/* * * * */
      case (sender, Proposal(color)) if myColor.exists(color ==) => response(tellSender = Reject(color))
/* * * * */
      case (sender, Proposal(color)) =>
        if(sender == name) throw up
        if(myColor.isEmpty && !state.isActive) self ! SendProposals
        response(tellSender = Accept(color))
/* * * * */
      case (sender, Accept(color)) if color == currentProposal => accepted(sender)
/* * * * */
      case (sender, Accept(_)) => response(tellSender = proposal)
/* * * * */
      case (sender, Reject(color)) if color == currentProposal =>
        nextProposal()
        acceptanceMap = createAcceptanceMap
        response(tellAll = proposal)
/* * * * */
      case (sender, Reject(_)) => response(tellSender = proposal) // it's outdated, send proposal
/* * * * */
      case (sender, Confirm(color)) =>
        colorOccupied(sender, color)
        if(myColor == Some(color)) ifHasNotLessNeighbors(sender){
          fallback()
        } getOrElse response(tellSender = Fallback())
        else response()
/* * * * */
      case (sender, Fallback()) =>
        ifHasNotLessNeighbors(sender)(fallback()) getOrElse response(tellSender = Fallback())
/* * * * */
      case (sender, NotifyFallback()) =>
        colorFreed(sender)
        response(tellSender = proposal)
/* * * * */
      case (sender, u) =>
        unhandled(u)
        response()
    }

    override def unhandled(message: Any) = alwaysReport("unhandled message: " + message)

    override def receive = {
      case GetStateInfo =>
        sender ! StateInfo(name, state.isActive, myColor, tries, currentProposal, acceptanceMap.toMap, neighbourColorMap.toMap)
      case SendProposals if neighbours.isEmpty => colorAccepted()
      case other => super.receive(other)
    }
  }


  case object GetStateInfo
  case class StateInfo(name: Name,
                       isActive: Boolean,
                       colorSet: Option[Color],
                       triesLeft: Seq[Color],
                       currentProposal: Color,
                       acceptanceMap: Map[Name, Boolean],
                       neighboursColorMap: Map[Name, Option[Color]])

}

trait ColoringAgentsImpl extends ColoringAgent{
  def actorProps = Props(classOf[ColoringActorAgent], name, controller, envRef, system.scheduler, neighbours)
}

trait DelayedColoring extends ColoringAgent{
  def messageDelay: FiniteDuration
  def proposalTickDelay: FiniteDuration
  def actorProps = Props(classOf[DelayedActorAgentImpl], name, controller, envRef, system.scheduler, neighbours, proposalTickDelay, messageDelay)
}

class GraphColoring(colors: Set[Color],
                    envGraph: ColoringGraph,
                    ensuringDoneDuration: FiniteDuration,
                    done: () => Unit)
                   (implicit system: ActorSystem, timeout: Timeout, val naming: Map[UUID, ColoringAgent#Id]){
  lazy val env = new ColoringEnvironment(colors, envGraph)

  protected def buildOverseer = new ColoringOverseer(env, ensuringDoneDuration, done)

  lazy val overseer = buildOverseer
  def overseerRef = overseer.actor

  def buildRef = ColoringEnvironmentRef(env, overseerRef, naming, timeout) _

  lazy val agentController = new ColoringAgentController(envGraph, naming)

  def createAgent(naming: Map[UUID, Name],
                  getNeighbours: UUID => Set[Name],
                  createEnvRef: UUID => ColoringEnvironmentRef,
                  tickDelay: FiniteDuration,
                  msgDelay: FiniteDuration)
                 (id: UUID) =
    new ColoringAgent(naming(id), getNeighbours(id), createEnvRef(id), agentController) with DelayedColoring
    {
      def messageDelay = msgDelay
      def proposalTickDelay = tickDelay
    }
}

trait ColoringGraphGenerator{


  def generate(name: String, nodes: Set[String], edges: Set[(String, String)]): ColoringGraph
  def generate(name: String, nNodes: Int, edge: (UUID, UUID) => Boolean): ColoringGraph
  def generate(name: String, nNodes: Int, edge: Random => Boolean, conf: RandConfig): ColoringGraph
}

object ColoringGraphGenerator{
  case class RandConfig(maxEdgesPerNode: Option[Int] = None)

  protected lazy val impl = new ColoringGraphGeneratorImpl

  def generate(name: String, nNodes: Int, edge: (Random) => Boolean, conf: RandConfig): ColoringGraph =
    impl.generate(name, nNodes, edge, conf)
}

class ColoringGraphGeneratorImpl extends ColoringGraphGenerator{
  def generate(name: String, nodes: Set[String], edges: Set[(String, String)]): ColoringGraph = ???

  def generate(name: String, nNodes: Int, edge: (UUID, UUID) => Boolean): ColoringGraph = ???


  def generate(name: String, nNodes: Int, edge: (Random) => Boolean, conf: RandConfig) = {
    val ref = GraphRef(name)
    val nodesIds = for(_ <- 1 to nNodes) yield UUID.randomUUID()
    val nodes = for(id <- nodesIds) yield Node[Option[Color]](ref, id, None)
    val edges = Y[(List[UUID], List[UUID]), List[(UUID, UUID)]](
      rec => {
        case (head :: tail, heads) =>
          heads.filter(_ => edge(Random)).map(head ->) ++ rec(tail, head +: heads)
        case (Nil, _) => Nil
      }
    )(nodesIds.toList -> Nil)

    val filteredEdges = conf.maxEdgesPerNode.map{
      max =>
        val edgesByNode = mutable.Map.empty[UUID, mutable.HashSet[(UUID, UUID)]]
          .withDefault(_ => mutable.HashSet())

        edges foreach {
          case e@(i1, i2) =>
            edgesByNode <<= (i1, _ += e)
            edgesByNode <<= (i2, _ += e)
        }

        val toDelete = edgesByNode.toSeq flatMap {
          case (k, v) if v.size > max =>
            v.toSeq.sortBy{
              case (i1, i2) => edgesByNode(i1).size min edgesByNode(i2).size
            }.toSeq |> {
              s =>
                s.take(s.size-max)
            }
          case _ => Nil
        }

        edges.toSet -- toDelete.toSet
      }
      .getOrElse(edges.toSet)

    ColoringGraph(ref, nodes.toSet, filteredEdges)
  }
}
