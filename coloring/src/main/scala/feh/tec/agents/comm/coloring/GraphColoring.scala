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
import feh.tec.agents.visualisation.util.Graph
import feh.dsl.swing.AppFrameControl
import scala.util.Random
import feh.tec.agents.coloring.util._
import scala.Some
import feh.tec.agents.visualisation.util.Graph.Node
import feh.tec.agents.comm.coloring.ColoringExpression.Accept
import feh.tec.agents.comm.coloring.ColoringExpression.Reject
import scala.concurrent.duration.FiniteDuration
import feh.tec.agent.comm.Agent.Communicating.AgentActor
import feh.tec.agents.comm.coloring.ColoringAgent.ColoringActorAgent
import feh.tec.agents.comm.coloring.OneTryColoring.OneTryColoringActorAgent

class ColoringEnvironment(val colors: Set[Color], envGraph: ColoringGraph) {
  env =>

  protected val _nodes = mutable.HashMap(envGraph.nodes.toSeq.map(tuple(_.id, identity)): _*)

  def nodes = _nodes.toMap

  def setColor(id: UUID, color: Option[Color]) = _nodes += id -> _nodes(id).newValue(color)
}

object ColoringOverseer{
  case class SetColor(node: UUID, color: Option[Color])
  case class GetColor(node: UUID)
  case object Done

  class OverseerActor(env: ColoringEnvironment) extends Actor{
    def receive: Actor.Receive = {
      case GetColor(nodeId) => env.nodes(nodeId).color
      case SetColor(nodeId, color) => env.setColor(nodeId, color)
      case Done => ???
    }
  }
}

class ColoringOverseer(val env: ColoringEnvironment)(implicit system: ActorSystem){
  protected def props = Props(classOf[ColoringOverseer.OverseerActor], env)
  lazy val actor = system.actorOf(props, "ColoringOverseer")
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

case class ColoringGraph(override val name: String, override val nodes: Set[ColoringGraph.Node]) extends Graph[Option[Color]](null, null)

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
  override def tellAll(msg: ColoringLanguage#Expr)(implicit sender: ActorRef): Unit =
    gr.neighbouringNodes(agents(id(sender)).envRef.nodeId).get.foreach(node => tell(naming(node.id), msg))
}

object ColoringAgent{
  protected case object Start
  //  protected case object Stop

  def start(agent: ActorRef) = agent ! Start

  abstract class ColoringActorAgent(name: Name,
                                    controller: CommAgentController[ColoringLanguage, ColoringEnvironment, _],
                                    envRef: ColoringEnvironmentRef,
                                    val scheduler: Scheduler,
                                    val selfActivationDelay: FiniteDuration)
    extends AgentActor[Name, ColoringLanguage, ColoringEnvironment](name, controller, envRef) with AgentReport
  {
    implicit def execContext = context.dispatcher

    protected var searchingColorToSet = false
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

    def scheduleActive = scheduler.scheduleOnce(selfActivationDelay, self, SendProposals)

    override def receive = super.receive orElse {
      case Start =>
        searchingColorToSet = true
        self ! SendProposals
      case SendProposals if searchingColorToSet =>
        controller.tellAll(proposal)
        scheduleActive
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
                             val controller: ColoringAgentController,
                             val selfActivationDelay: FiniteDuration)
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

  class OneTryColoringActorAgent(name: Name,
                                 controller: CommAgentController[ColoringLanguage, ColoringEnvironment, _],
                                 envRef: ColoringEnvironmentRef,
                                 scheduler: Scheduler,
                                 selfActivationDelay: FiniteDuration,
                                 val neighbours: Set[Name])
    extends ColoringActorAgent(name, controller, envRef, scheduler, selfActivationDelay)
  {
    protected var tries = envRef.possibleColors

    var currentProposal = tries.head
    def setProposal() = currentProposal = tries.head
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
      _freeColors = calcFreeColors
    }
    // assuming no myColor is set
    def calcFreeColors = envRef.possibleColors &~ neighbourColorMap.values.flatten.toSet

    var _freeColors = envRef.possibleColors
    def freeColors = _freeColors

    def fallback() = {
      tries = tries.tail
      if(tries.isEmpty) sys.error("no more colors to try")
      setProposal()
      acceptanceMap = createAcceptanceMap
      _freeColors = calcFreeColors
      if(!searchingColorToSet) ColoringAgent start self
      response(tellAll = NotifyFallback())
    }

    override def setColor(c: Option[Color]): Unit = {
      super.setColor(c)
      c.foreach(_freeColors -= _)
    }

    def accepted(by: Name) = {
      acceptanceMap += by -> true
      if(allAccepted_?) {
        searchingColorToSet = false
        setColor(Some(currentProposal.ensuring(_ != null)))
        response(tellAll = Confirm(currentProposal)) $$ {
          currentProposal = null
        }
      }
      else response()
    }

    def respond = {
      case (sender, Proposal(color)) if freeColors.contains(color) => response(tellSender = Accept(color))
      case (sender, Proposal(color)) => response(tellSender = Reject(color, freeColors))
      case (sender, Accept(color)) if color == currentProposal => accepted(sender)
      case (sender, Reject(color, colors)) if color == currentProposal =>
        val inters = freeColors & colors
        if(inters.isEmpty) fallback()
        else {
          currentProposal = inters.randomChoose
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
        Response()
      case (sender, u) =>
        unhandled(u)
        response()
    }

    override def unhandled(message: Any) = report("unhandled message: " + message)
  }

}

trait OneTryColoring extends ColoringAgent{
  def actorProps = Props(classOf[OneTryColoringActorAgent], name, controller, envRef, system.scheduler, selfActivationDelay, neighbours)
}

class GraphColoring(colors: Set[Color], envGraph: ColoringGraph, selfActivation: FiniteDuration, scheduler: Scheduler)
                   (implicit system: ActorSystem, timeout: Timeout, naming: UUID => ColoringAgent#Id){
  lazy val env = new ColoringEnvironment(colors, envGraph)

  protected def buildOverseer = new ColoringOverseer(env)

  lazy val overseer = buildOverseer
  def overseerRef = overseer.actor

  def buildRef = ColoringEnvironmentRef(env, overseerRef, timeout) _

  lazy val agentController = new ColoringAgentController(envGraph, naming)

  def agentProps(naming: Map[UUID, Name], getNeighbours: UUID => Set[Name], id: UUID) =
    Props(classOf[ColoringAgent], naming(id), getNeighbours(id), buildRef(id), agentController)
  def createAgent(naming: Map[UUID, Name], getNeighbours: UUID => Set[Name], createEnvRef: UUID => ColoringEnvironmentRef)
                 (id: UUID, selfActivation: FiniteDuration) =
    new ColoringAgent(naming(id), getNeighbours(id), createEnvRef(id), agentController, selfActivation) with OneTryColoring

//  val agents = envGraph.nodes.map(createAgent)
}

trait GraphColoringVisualisation extends AppFrameControl{
  protected def graph: ColoringGraph

  def update(id: UUID, color: Option[Color])
}


trait GraphGenerator{
  def generate(name: String, nodes: Set[String], edges: Set[(String, String)]): ColoringGraph
  def generate(name: String, nNodes: Int, edge: (UUID, UUID) => Boolean): ColoringGraph
  def generate(name: String, nNodes: Int, edge: Random => Boolean): ColoringGraph
}

class GraphGeneratorImpl extends GraphGenerator{
  def generate(name: String, nodes: Set[String], edges: Set[(String, String)]): ColoringGraph = ???

  def generate(name: String, nNodes: Int, edge: (UUID, UUID) => Boolean): ColoringGraph = ???

  def generate(name: String, nNodes: Int, edge: (Random) => Boolean): ColoringGraph = {
    val nodesIds = for(_ <- 1 to nNodes) yield UUID.randomUUID()
    val nodes = for {
      id <- nodesIds
      neigh = nodesIds.filter(e => e != id && edge(Random))
    } yield Node[Option[Color]](id, None, neigh.toSet)
    ColoringGraph(name, nodes.toSet)
  }
}
