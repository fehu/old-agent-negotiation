package feh.tec.agents.comm.coloring

import feh.tec.agent.comm._
import java.awt.Color
import java.util.UUID
import feh.util._
import scala.collection.mutable
import feh.tec.agent.comm.Agent.{Communicating, CommAgentController}
import akka.actor._
import akka.util.Timeout
import akka.pattern._
import scala.concurrent.ExecutionContext
import ColoringGraph._
import feh.tec.agents.comm.coloring.ColoringOverseer._
import feh.tec.agents.comm.coloring.ColoringExpression.Fallback
import feh.tec.agents.visualisation.util.Graph
import feh.dsl.swing.AppFrameControl
import scala.util.Random
import feh.tec.agents.visualisation.util.Graph.Node
import feh.tec.agents.comm.coloring.ColoringOverseer.GetColor
import feh.tec.agents.comm.coloring.ColoringExpression.Accept
import feh.tec.agents.comm.coloring.ColoringOverseer.SetColor
import feh.tec.agents.coloring.util._
import feh.tec.agents.comm.coloring.ColoringExpression.Proposal
import scala.Some
import feh.tec.agents.comm.coloring.ColoringOverseer.GetColor
import feh.tec.agents.visualisation.util.Graph.Node
import feh.tec.agents.comm.coloring.ColoringExpression.Accept
import feh.tec.agents.comm.coloring.ColoringExpression.Reject
import feh.tec.agents.comm.coloring.ColoringOverseer.SetColor
import scala.concurrent.duration.FiniteDuration

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
}
class ColoringOverseer(val id: UUID, val env: ColoringEnvironment) extends Actor{
  def receive: Actor.Receive = {
    case GetColor(nodeId) => env.nodes(nodeId).color
    case SetColor(nodeId, color) => env.setColor(nodeId, color)
    case Done => ???
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

case class ColoringGraph(override val name: String, override val nodes: Set[ColoringGraph.Node]) extends Graph[Option[Color]](null, null)

class ColoringLanguage extends NegotiatingLanguage{
  type Expr = ColoringExpression
  type Issue = ColoringIssue
  type Accepted = Accept
  type Rejected = Fallback.type
}

trait ColoringExpression{
  def asText: String

  override def toString: String = super.toString + s"[$asText]"
}
trait ColoringIssue extends ColoringExpression

object ColoringExpression{
  case class Proposal(color: Color) extends ColoringIssue{
    def asText = "I propose I set this color"
  }
  case class Reject(offer: Color, colors: Set[Color]) extends ColoringIssue{
    def asText = "Your offer is unacceptable, those are colors I can set"
  }
  case class Accept(color: Color) extends ColoringExpression{
    def asText = "I'm ok with your color assignation"
  }
  case class Confirm(color: Color) extends ColoringExpression{
    def asText = "I confirm my color assignation"
  }
  case object Fallback extends ColoringExpression{
    def asText = "I have no options to assign"
  }
  case object NotifyFallback extends ColoringExpression{
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
}

object AgentReport{
  object default{
    var enabled = false 
  }
}
trait AgentReport {
  self: Communicating[_, _] =>

  object report{
    var enabled = AgentReport.default.enabled
  }
  
  def report(msg: String): Unit = if(report.enabled) println(s"[$id] " + msg.replaceAllLiterally("\n", "\n" + " " * (id.toString.length + 3)))
}

abstract class ColoringAgent(val name: Name, 
                             val neighbours: Set[Name],
                             val envRef: ColoringEnvironmentRef,
                             val controller: ColoringAgentController,
                             val selfActivationDelay: FiniteDuration,
                             protected val scheduler: Scheduler)
  extends Agent.Communicating[ColoringLanguage, ColoringEnvironment]  with AgentReport
{
  type EnvRef = ColoringEnvironmentRef

  type Id = Name
  val id = name
  
  controller.register(this)

  import ColoringAgent._
  
  def proposalFor: Proposal

  var myColor: Option[Color] = None

//  def tellAllNeighbours(msg: Name => ColoringLanguage#Expr) = neighbours.map(id => controller.tell(id, msg(id)))

  implicit def execContext = context.dispatcher

  def isLangExpr(a: Any) = a.isInstanceOf[ColoringLanguage#Expr]

  def onStart()
  
  def setColor(c: Option[Color]){
    myColor = c
    envRef.setColor(c).foreach{ _ =>
      report(s"Set color of my node to $c / ${c.map(_.hexRGB).getOrElse("")}")
    }
  }

  protected case object SendProposals

  var searchingColorToSet = false
  override def receive: Actor.Receive = super.receive orElse {
    case Start =>
      searchingColorToSet = true
      onStart()
      self ! SendProposals
    case SendProposals if searchingColorToSet =>
      controller.tellAll(proposalFor)
      scheduleActive
  }

  def scheduleActive = scheduler.scheduleOnce(selfActivationDelay, self, SendProposals)

}

trait OneTryColoring extends ColoringAgent{

//  protected var tries = envRef.possibleColors
  protected var tries = envRef.possibleColors

  var currentProposal = tries.head
  def proposalFor(neighbour: Name) = proposal
  def setProposal() = currentProposal = tries.head
  def proposal = Proposal(currentProposal)

  def createAcceptanceMap = mutable.Map(neighbours.toSeq.zipMap(_ => false): _*)
  var acceptanceMap = createAcceptanceMap
  def allAccepted_? = acceptanceMap forall (_._2)

  val neighbourColorMap = mutable.Map(neighbours.toSeq.zipMap(_ => Option.empty[Color]): _*)

  def colorOccupied(by: Id, c: Color)  {
    neighbourColorMap += by -> Some(c)
    _freeColors = calcFreeColors
  }
  def colorFreed(by: Id) {
    neighbourColorMap += by -> None
    _freeColors = calcFreeColors
  }
  // assuming no myColor is set
  def calcFreeColors = envRef.possibleColors &~ neighbourColorMap.values.flatten.toSet

  var _freeColors = envRef.possibleColors
  def freeColors = _freeColors

  def fallback() = {
    setColor(None)
    tries = tries.tail
    if(tries.isEmpty) sys.error("no more colors to try")
    setProposal()
    acceptanceMap = createAcceptanceMap
    _freeColors = calcFreeColors
    if(!searchingColorToSet) self ! SendProposals
    Response(tellAll = NotifyFallback)
  }

  override def setColor(c: Option[Color]): Unit = {
    super.setColor(c)
    c.foreach(_freeColors -= _)
  }

  def accepted(by: Id) = {
    acceptanceMap += by -> true
    if(allAccepted_?) {
      setColor(myColor)
      currentProposal = null
      searchingColorToSet = false
      Response(tellAll = Confirm(currentProposal))
    }
    else Response()
  }

  def respond = {
    case Proposal(color) if freeColors.contains(color) => Response(tellSender = Accept(color))
    case Proposal(color) => Response(tellSender = Reject(color, freeColors))
    case Accept(color) if color == currentProposal => accepted(controller.id(sender))
    case Reject(color, colors) if color == currentProposal =>
      val inters = freeColors & colors
      if(inters.isEmpty) fallback()
      else {
        currentProposal = inters.randomChoose
        Response(tellAll = proposal)
      }
    case Confirm(color) =>
      if(freeColors contains color) {
        colorOccupied(controller.id(sender), color)
        Response()
      }
      else Response(tellSender = Fallback)
    case Fallback => fallback()
    case NotifyFallback =>
      colorFreed(controller.id(sender))
      Response()
    //    case (Propose(color), env) if notTried.contains(color) =>
    //      env.setColor(color)
    //      myColor = Option(color)
    //      notTried -= color
    //      Some(Accept(color))
    //    case (Propose(color), env) => Some(Reject(notTried.toSet))
    //    case (Reject(colors), env) =>
    //      val diff = colors & notTried
    //      if(diff.isEmpty) {
    //        tellAllNeighbours( _=> Fallback)
    //        None
    //      }
    //      else { // todo
    //        notTried = diff
    //        Some(proposalFor(controller.id(sender)))
    //      }
    //    case (Accept(color), env) if notTried contains color =>
    //      notTried -= color
    //      None
    //    case (Accept(color), env) => Some(Fallback)
    //    case (Fallback, env) =>
    //      myColor = None
    //      notTried = envRef.possibleColors
    //      None
  }
}

class GraphColoring(colors: Set[Color], envGraph: ColoringGraph)
                   (implicit system: ActorSystem, timeout: Timeout, naming: UUID => ColoringAgent#Id){
  val env = new ColoringEnvironment(colors, envGraph)

  val overseerId = UUID.randomUUID()
  def overseerProps = Props(classOf[ColoringOverseer], overseerId, env)
  val overseerRef = system.actorOf(overseerProps)

  def buildRef = ColoringEnvironmentRef(env, overseerRef, timeout) _

  val agentController = new ColoringAgentController(envGraph, naming)

  def agentProps(naming: Map[UUID, Name], getNeighbours: UUID => Set[Name], id: UUID) =
    Props(classOf[ColoringAgent], naming(id), getNeighbours(id), buildRef(id), agentController)
  def createAgent(naming: Map[UUID, Name], getNeighbours: UUID => Set[Name])(id: UUID) =
    system.actorOf(agentProps(naming, getNeighbours, id))

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
