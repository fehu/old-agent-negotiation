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
import feh.tec.comm.util.Graph
import ColoringGraph._
import feh.tec.agents.comm.coloring.ColoringOverseer._
import feh.tec.agents.comm.coloring.ColoringExpression.{Fallback, Accept}

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

case class ColoringGraph(override val nodes: Set[Node]) extends Graph(nodes)

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

class ColoringAgent(val node: Node,
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

  def agentProps(node: Node) = Props(classOf[ColoringAgent], node, buildRef(node.id), agentController)
  def createAgent(node: Node) = system.actorOf(agentProps(node))

  val agents = envGraph.nodes.map(createAgent)
}