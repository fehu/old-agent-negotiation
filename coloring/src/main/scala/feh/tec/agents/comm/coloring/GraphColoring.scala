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
import scala.concurrent.duration.FiniteDuration
import feh.tec.agent.comm.Agent.Communicating.{ResponseDelay, AgentActor}
import feh.tec.agents.comm.coloring.ColoringAgent.ColoringActorAgent
import feh.tec.agents.comm.coloring.OneTryColoring.OneTryDelayedActorAgent
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
  case object Done

  class OverseerActor(env: ColoringEnvironment) extends Actor{
    def receive: Actor.Receive = {
      case GetColor(nodeId) => sender ! env.nodes(nodeId).color
      case SetColor(nodeId, color) => env.setColor(nodeId, color)
      case GetColors => sender ! env.nodes
      case GetNNeighbours(id) => env.graph.byId(id).foreach(n => sender ! NNeighbours(n.id, n.neighbours.size))
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

    def onStart() {}

    override def receive = super.receive orElse {
      case Start =>
        report("I've started")
        self ! SendProposals
        onStart()
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

object OneTryColoring{
  protected def response = Response.build[ColoringLanguage, ColoringEnvironment]

  class OneTryDelayedActorAgent(name: Name,
                                controller: CommAgentController[ColoringLanguage, ColoringEnvironment, _],
                                envRef: ColoringEnvironmentRef,
                                scheduler: Scheduler,
                                neighbours: Set[Name],
                                proposalTickDelay: FiniteDuration,
                                val messageDelay: FiniteDuration)
    extends OneTryColoringActorAgent(name, controller, envRef, scheduler, proposalTickDelay, neighbours)
    with ResponseDelay[Name, ColoringLanguage, ColoringEnvironment]

  class OneTryColoringActorAgent(name: Name,
                                 controller: CommAgentController[ColoringLanguage, ColoringEnvironment, _],
                                 envRef: ColoringEnvironmentRef,
                                 scheduler: Scheduler,
                                 proposalTickDelay: FiniteDuration,
                                 val neighbours: Set[Name])
    extends ColoringActorAgent(name, controller, envRef, scheduler)
  {
    var tries = envRef.possibleColors.toSeq
    lazy val nNeighbours = neighbours.size 
    lazy val nNeighboursConnections = neighbours.zipMap(envRef.reverseNaming).toMap.par.mapValues{
      id => Await.result(envRef.nConnections(id), 200 millis)
    }.toMap
    
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

    def ignoreFallbackRequestCoeff = .4
    def ignoreFallbackRequest =
      ignoreFallbackRequestCoeff * (1 - tries.size.toDouble / envRef.possibleColors.size) > Random.nextDouble()

    def ifHasNotLessNeighbors[R](sender: Name)(f: => Response[ColoringLanguage, ColoringEnvironment]) =
      nNeighboursConnections.get(sender).map{
        n => 
          if(n >= nNeighbours && !ignoreFallbackRequest) Some(f)
          else None
      }.getOrElse{
        sys.error(s"$sender is not neighbour of $name")
      }
    
    def respond = {
/* * * * */
      case (sender, Proposal(color)) if freeColors.contains(color) =>
        if(sender == name) throw up
        if(myColor.isEmpty && !isActive) self ! SendProposals
        response(tellSender = Accept(color))
/* * * * */
      case (sender, Proposal(color)) => response(tellSender = Reject(color, freeColors))
/* * * * */
      case (sender, Accept(color)) if color == currentProposal => accepted(sender)
/* * * * */
      case (sender, Accept(_)) => response(tellSender = proposal)
/* * * * */
      case (sender, Reject(color, colors)) if color == currentProposal =>
        val inters = freeColors & colors
        ifHasNotLessNeighbors(sender){
          if(inters.isEmpty) fallback()
          else {
            currentProposal = inters.randomChoose
            acceptanceMap = createAcceptanceMap
            response(tellAll = proposal)
          }
        } getOrElse response()
/* * * * */
      case (sender, Reject(_, _)) => response() // ignore, it's outdated
/* * * * */
      case (sender, Confirm(color)) =>
        colorOccupied(sender, color)
        if(freeColors contains color) Response()
        else if(myColor == Some(color)) ifHasNotLessNeighbors(sender){
          fallback()
        } getOrElse response(tellSender = Fallback())
        else response(tellSender = Fallback())
/* * * * */
      case (sender, Fallback()) =>
        ifHasNotLessNeighbors(sender)(fallback()) getOrElse response()
/* * * * */
      case (sender, NotifyFallback()) =>
        colorFreed(sender)
        if(acceptanceMap.forall(_._2)) Response()
        else response(tellSender = proposal)
/* * * * */
      case (sender, u) =>
        unhandled(u)
        response()
    }

    override def unhandled(message: Any) = alwaysReport("unhandled message: " + message)

    case object Tick

    def startTicks() = scheduler.schedule(0 millis, proposalTickDelay, self, Tick)

    override def onStart() = {
      super.onStart()
      startTicks()
    }

    override def receive = {
      case Tick =>
        if(!acceptanceMap.forall(_._2)) {
          println("tick proposals")
          self ! SendProposals
        }
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
  def proposalTickDelay: FiniteDuration
  def actorProps = Props(classOf[OneTryDelayedActorAgent], name, controller, envRef, system.scheduler, neighbours, proposalTickDelay, messageDelay)
}

class GraphColoring(colors: Set[Color], envGraph: ColoringGraph)
                   (implicit system: ActorSystem, timeout: Timeout, val naming: Map[UUID, ColoringAgent#Id]){
  lazy val env = new ColoringEnvironment(colors, envGraph)

  protected def buildOverseer = new ColoringOverseer(env)

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
    new ColoringAgent(naming(id), getNeighbours(id), createEnvRef(id), agentController) with OneTryDelayedColoring
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
