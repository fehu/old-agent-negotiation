package feh.tec.agents.comm.coloring

import feh.tec.agent.comm._
import java.awt.Color
import java.util.UUID
import feh.util._
import scala.collection.mutable
import feh.tec.agent.comm.Agent.CommAgentController
import akka.actor.{ActorSystem, Props, ActorRef, Actor}
import akka.util.Timeout
import akka.pattern._
import scala.concurrent.ExecutionContext
import ColoringGraph._
import feh.tec.agents.comm.coloring.ColoringOverseer._
import feh.tec.agents.comm.coloring.ColoringExpression.Fallback
import feh.tec.agents.visualisation.util.Graph
import feh.dsl.swing.AppFrameControl
import scala.util.Random
import scala.Some
import feh.tec.agents.visualisation.util.Graph.Node
import feh.tec.agents.comm.coloring.ColoringOverseer.GetColor
import feh.tec.agents.comm.coloring.ColoringExpression.Accept
import feh.tec.agents.comm.coloring.ColoringOverseer.SetColor

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
  def setColor(color: Color)(implicit context: ExecutionContext) = overseerRef ? SetColor(nodeId, Option(color)) map(_ => {})
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
}
trait ColoringIssue extends ColoringExpression

object ColoringExpression{
  case class Propose(color: Color) extends ColoringIssue{
    def asText = "I propose I set this color"
  }
  case class Reject(colors: Set[Color]) extends ColoringIssue{
    def asText = "Your offer is unacceptable, those are colors I can set"
  }
  case class Accept(color: Color) extends ColoringExpression{
    def asText = "I confirm with your color assignation"
  }
  case object Fallback extends ColoringExpression{
    def asText = "I have no options to assign"
  }
}

import ColoringExpression._

class ColoringAgentController extends CommAgentController[ColoringLanguage, ColoringEnvironment, ColoringAgent]

class ColoringAgent(val node: ColoringGraph.Node,
                    val envRef: ColoringEnvironmentRef,
                    val controller: ColoringAgentController)
  extends Agent.Communicating[ColoringLanguage, ColoringEnvironment] with Actor
{
  type EnvRef = ColoringEnvironmentRef

  val id = node.id
  
  controller.register(this)

  protected case object Start

  val neighbours = node.neighbours

  protected var canAssign = envRef.possibleColors
  
  def proposalFor(neighbour: UUID) = Propose(canAssign.head)   

  var myColor: Option[Color] = None

  def tellAllNeighbours(msg: UUID => ColoringLanguage#Expr) = neighbours.map(id => controller.tell(id, msg(id)))

  implicit def execContext = context.dispatcher

  def respond = {
    case (Propose(color), env) if canAssign.contains(color) =>
      env.setColor(color)
      myColor = Option(color)
      canAssign -= color
      Some(Accept(color))
    case (Propose(color), env) => Some(Reject(canAssign.toSet))
    case (Reject(colors), env) =>
      val diff = colors & canAssign
      if(diff.isEmpty) {
        tellAllNeighbours( _=> Fallback)
        None
      }
      else { // todo
        canAssign = diff
        Some(proposalFor(controller.id(sender)))
      }
    case (Accept(color), env) if canAssign contains color =>
      canAssign -= color
      None
    case (Accept(color), env) if canAssign contains color => Some(Fallback)
    case (Fallback, env) =>
      myColor = None
      canAssign = envRef.possibleColors
      None
  }

  override def receive: Actor.Receive = super.receive orElse{
    case Start => tellAllNeighbours(proposalFor)
  }

}

class GraphColoring(colors: Set[Color], envGraph: ColoringGraph)(implicit system: ActorSystem, timeout: Timeout){
  val env = new ColoringEnvironment(colors, envGraph)

  val overseerId = UUID.randomUUID()
  val overseerProps = Props(classOf[ColoringOverseer], overseerId, env)
  val overseerRef = system.actorOf(overseerProps)

  def buildRef = ColoringEnvironmentRef(env, overseerRef, timeout) _

  val agentController = new ColoringAgentController

  def agentProps(node: ColoringGraph.Node) = Props(classOf[ColoringAgent], node, buildRef(node.id), agentController)
  def createAgent(node: ColoringGraph.Node) = system.actorOf(agentProps(node))

  val agents = envGraph.nodes.map(createAgent)
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
