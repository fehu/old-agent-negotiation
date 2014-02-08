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
import scala.concurrent.ExecutionContext
import ColoringGraph._
import feh.tec.agents.comm.coloring.ColoringOverseer._
import feh.util.{Undirected, Graph}
import scala.util.Random
import feh.tec.agents.coloring.util._
import feh.util.Graph.{GraphRef, Node}
import feh.tec.agents.comm.coloring.ColoringExpression.Accept
import feh.tec.agents.comm.coloring.ColoringExpression.Reject
import scala.concurrent.duration.FiniteDuration
import feh.tec.agent.comm.Agent.Communicating.{ResponseDelay, AgentActor}
import feh.tec.agents.comm.coloring.ColoringAgent.ColoringActorAgent
import feh.tec.agents.comm.coloring.OneTryColoring.OneTryDelayedActorAgent
import feh.tec.agents.comm.coloring.ColoringGraphGenerator.RandConfig

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
  case object Done

  class OverseerActor(env: ColoringEnvironment) extends Actor{
    def receive: Actor.Receive = {
      case GetColor(nodeId) => sender ! env.nodes(nodeId).color
      case SetColor(nodeId, color) => env.setColor(nodeId, color)
      case GetColors => sender ! env.nodes 
      case Done => ???
    }
  }
}

class ColoringOverseer(val env: ColoringEnvironment)(implicit system: ActorSystem, timeout: Timeout){
  protected def props = Props(classOf[ColoringOverseer.OverseerActor], env)
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
  def apply(env: ColoringEnvironment, overseerRef: ActorRef, timeout: Timeout)(node: UUID) =
    new ColoringEnvironmentRef(node, env.colors, overseerRef, timeout)
}

class ColoringEnvironmentRef(val nodeId: UUID, 
                             val possibleColors: Set[Color],
                             protected val overseerRef: ActorRef,
                             implicit val timeout: Timeout)
  extends EnvironmentRef[ColoringEnvironment]
{
  def setColor(color: Option[Color])(implicit context: ExecutionContext) = overseerRef ? SetColor(nodeId, color) map(_ => {})
  def color(implicit context: ExecutionContext) = (overseerRef ? GetColor(nodeId)).mapTo[Option[Color]]
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
  case class Reject(offer: Color, colors: Set[Color])(implicit val sender: ActorRef) extends ColoringIssue{
    def asText = s"Your offer ($offer) is unacceptable, those are colors I can set: $colors"
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

class ColoringAgentController(val gr: ColoringGraph, naming: UUID => ColoringAgent#Id) extends CommAgentController[ColoringLanguage, ColoringEnvironment, ColoringAgent]{
  override def tellAll(msg: ColoringLanguage#Expr)(implicit sender: ActorRef): Unit ={
    val neigh = gr.neighbouringNodes(agents(id(sender)).envRef.nodeId).map(node => naming(node.id))
    neigh foreach (tell(_, msg))
  }
}

object ColoringAgent{
  protected case object Start
  //  protected case object Stop

  def start(agent: ActorRef) = agent ! Start

  abstract class ColoringActorAgent(name: Name,
                                    controller: CommAgentController[ColoringLanguage, ColoringEnvironment, _],
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

    var isActive = false

    override def receive = super.receive orElse {
      case Start =>
        report("I've started")
        self ! SendProposals
      case SendProposals =>
        isActive = true
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
  
  def report(msg: String): Unit = if(report.enabled) println(s"[$id] " + msg.replaceAllLiterally("\n", "\n" + " " * (id.toString.length + 3)))
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

object OneTryColoring{
  protected def response = Response.build[ColoringLanguage, ColoringEnvironment]

  class OneTryDelayedActorAgent(name: Name,
                                controller: CommAgentController[ColoringLanguage, ColoringEnvironment, _],
                                envRef: ColoringEnvironmentRef,
                                scheduler: Scheduler,
                                neighbours: Set[Name],
                                val messageDelay: FiniteDuration)
    extends OneTryColoringActorAgent(name, controller, envRef, scheduler, neighbours)
    with ResponseDelay[Name, ColoringLanguage, ColoringEnvironment]

  class OneTryColoringActorAgent(name: Name,
                                 controller: CommAgentController[ColoringLanguage, ColoringEnvironment, _],
                                 envRef: ColoringEnvironmentRef,
                                 scheduler: Scheduler,
//                                 proposalTickDelay: FiniteDuration,
                                 val neighbours: Set[Name])
    extends ColoringActorAgent(name, controller, envRef, scheduler)
  {
    protected var tries = envRef.possibleColors.toSeq

    var currentProposal = tries.head
    def nextProposal() = currentProposal = {
      tries match{
        case Nil =>
          tries = envRef.possibleColors.toSeq
        case _ =>
      }
      val r = tries.head
      tries = tries.tail
      r
    }
    def proposal = Proposal(currentProposal)

    def createAcceptanceMap = mutable.Map(neighbours.toSeq.zipMap(_ => false): _*)
    var acceptanceMap = createAcceptanceMap
    def allAccepted_? = acceptanceMap forall (_._2)

    val neighbourColorMap = mutable.Map(neighbours.toSeq.zipMap(_ => Option.empty[Color]): _*)

    def colorOccupied(by: Name, c: Color)  {
      neighbourColorMap += by -> Some(c)
      _freeColors = calcFreeColors
    }
    def colorFreed(by: Name) {
      neighbourColorMap += by -> None
      acceptanceMap += by -> false
      _freeColors = calcFreeColors
    }
    // assuming no myColor is set
    def calcFreeColors = envRef.possibleColors &~ neighbourColorMap.values.flatten.toSet

    var _freeColors = calcFreeColors
    def freeColors = _freeColors

    def fallback() = {
      setColor(None)
//      if(currentProposal != null) tries = tries.tail
//      if(tries.isEmpty) sys.error("no more colors to try") // tries = envRef.possibleColors.toSeq
      nextProposal()
      acceptanceMap = createAcceptanceMap
      _freeColors = calcFreeColors
      self ! SendProposals
      response(tellAll = NotifyFallback())
    }

    override def setColor(c: Option[Color]): Unit = {
      super.setColor(c)
      c.foreach(_freeColors -= _)
    }

    def accepted(by: Name) = {
      acceptanceMap += by -> true
      if(allAccepted_?) {
        colorAccepted()
      }
      else response()
    }

    def colorAccepted() = {
      setColor(Some(currentProposal.ensuring(_ != null)))
      tries = tries.filterNot(currentProposal ==)
      response(tellAll = Confirm(currentProposal)) $$ {
        currentProposal = null
        isActive = false
      }
    }

    def respond = {
      case (sender, Proposal(color)) if freeColors.contains(color) =>
        if(sender == name) throw up
        if(myColor.isEmpty && !isActive) self ! SendProposals
        response(tellSender = Accept(color))
      case (sender, Proposal(color)) => response(tellSender = Reject(color, freeColors))
      case (sender, Accept(color)) if color == currentProposal => accepted(sender)
      case (sender, Reject(color, colors)) if color == currentProposal =>
        val inters = freeColors & colors
        if(inters.isEmpty) fallback()
        else {
          currentProposal = inters.randomChoose
          acceptanceMap = createAcceptanceMap
          response(tellAll = proposal)
        }
      case (sender, Confirm(color)) =>
        if(freeColors contains color) {
          colorOccupied(sender, color)
          Response()
        }
        else response(tellSender = Fallback())
      case (sender, Fallback()) => fallback()
      case (sender, NotifyFallback()) =>
        colorFreed(sender)
        if(acceptanceMap.forall(_._2)) Response()
        else response(tellSender = proposal)
      case (sender, u) =>
        unhandled(u)
        response()
    }

    override def unhandled(message: Any) = report("unhandled message: " + message)

    override def receive = {
      case GetStateInfo =>
        sender ! StateInfo(name, isActive, myColor, tries, currentProposal, acceptanceMap.toMap, freeColors, neighbourColorMap.toMap)
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
                       freeColors: Set[Color],
                       neighboursColorMap: Map[Name, Option[Color]])

}

trait OneTryColoring extends ColoringAgent{
  def actorProps = Props(classOf[ColoringActorAgent], name, controller, envRef, system.scheduler, neighbours)
}

trait OneTryDelayedColoring extends ColoringAgent{
  def messageDelay: FiniteDuration
  def actorProps = Props(classOf[OneTryDelayedActorAgent], name, controller, envRef, system.scheduler, neighbours, messageDelay)
}

class GraphColoring(colors: Set[Color], envGraph: ColoringGraph)
                   (implicit system: ActorSystem, timeout: Timeout, naming: UUID => ColoringAgent#Id){
  lazy val env = new ColoringEnvironment(colors, envGraph)

  protected def buildOverseer = new ColoringOverseer(env)

  lazy val overseer = buildOverseer
  def overseerRef = overseer.actor

  def buildRef = ColoringEnvironmentRef(env, overseerRef, timeout) _

  lazy val agentController = new ColoringAgentController(envGraph, naming)

  def createAgent(naming: Map[UUID, Name],
                  getNeighbours: UUID => Set[Name],
                  createEnvRef: UUID => ColoringEnvironmentRef,
                  msgDelay: FiniteDuration)
                 (id: UUID) =
    new ColoringAgent(naming(id), getNeighbours(id), createEnvRef(id), agentController) with OneTryDelayedColoring
    {
      def messageDelay = msgDelay
    }
}

trait ColoringGraphGenerator{


  def generate(name: String, nodes: Set[String], edges: Set[(String, String)]): ColoringGraph
  def generate(name: String, nNodes: Int, edge: (UUID, UUID) => Boolean): ColoringGraph
  def generate(name: String, nNodes: Int, edge: Random => Boolean, conf: RandConfig): ColoringGraph
}

object ColoringGraphGenerator{
  case class RandConfig(maxEdgesPerNode: Option[Int])

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
