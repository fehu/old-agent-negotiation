package feh.tec.agents.coloring.visual

import feh.tec.agents.comm.coloring.{ColoringAgent, GraphColoring, ColoringGraph}
import java.awt.Color
import scala.concurrent.duration.FiniteDuration
import akka.util.Timeout
import akka.actor.ActorSystem
import feh.tec.agents.coloring.util.NameGenerator
import java.util.UUID
import feh.util._

abstract class ColoringVisualizationApplication(val graph: ColoringGraph,
                                                colors: Set[Color],
                                                val msgDelay: FiniteDuration,
                                                implicit val defaultTimeout: Timeout)
                                               (implicit val system: ActorSystem = ActorSystem.create()){
  lazy val generator = new NameGenerator(Set())
  implicit lazy val naming = graph.nodes.toSeq.map(_.id -> generator.nextName).toMap

  def visual: GraphColoringVisualisation

  lazy val updaterRef = ColorUpdateActor.actor(visual)
  lazy val env = new GraphColoring(colors, graph){
    override protected def buildOverseer = new ColoringUpdateOverseer(env, updaterRef)
  }
  protected def getNeighbours(id: UUID) = graph.neighbouringNodes(id).map(_.id |> naming)

  def createAgent = env.createAgent(naming, getNeighbours, env.buildRef, msgDelay) _

  lazy val agents = naming.keys.map(createAgent).toList
  lazy val starting = agents.randomChoice

  def start(){
    visual.start()
    println("starting: " + starting)
    ColoringAgent start starting.actor
  }
}
