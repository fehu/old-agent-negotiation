package feh.tec.agents.coloring.visual

import feh.tec.agents.comm.coloring.{GraphColoring, ColoringGraphGenerator, ColoringGraph}
import java.util.UUID
import feh.tec.agents.coloring.util.Name
import edu.uci.ics.jung.graph.UndirectedSparseGraph
import edu.uci.ics.jung.algorithms.layout.KKLayout
import java.awt.Color
import edu.uci.ics.jung.visualization.VisualizationViewer
import scala.concurrent.duration._
import akka.actor.ActorSystem

class JUNGVisualizationApp(val graph: ColoringGraph,
                           naming: UUID => Name)
  extends GraphColoringVisualisation with JUNGVisualizationBuilder
{

  lazy val vis = new JUNGBaseVisualization(new UndirectedSparseGraph, new KKLayout(_))(graph, naming)

  def update(name: Name, color: Option[Color]) = { vis.updateColor(name, color)}
  def update(id: UUID, color: Option[Color]) = update(naming(id), color)

  def start() = vis.start()
  def stop() = vis.stop()

  def frame = vis
  type Visualization = VisualizationViewer[Name, (Name, Name)]
  def graphVisualization = vis.vv
}

class JUNGVisualizationAppExtra(graph: ColoringGraph, naming: UUID => Name, env: GraphColoring)
                               (implicit asystem: ActorSystem)
  extends JUNGVisualizationApp(graph, naming) with JUNGVisualizationBuilderExtraLayoutImpl
{
  override lazy val vis = new JUNGBaseVisualization(new UndirectedSparseGraph, null)(graph, naming)
    with JUNGVisualizationFeatures with JUNGInfoVisualization
  {
    lazy val infoExtractor = new OneTryColoringStateInfoExtractor(env, 10 millis)
    protected implicit def system = asystem
  }

}


object JUNGVisualizationApplication extends App{
  val app = new ColoringVisualizationApplication(
    graph = ColoringGraphGenerator.generate("coloring", 30, _.nextDouble() < .1),
    colors = Set(Color.red, Color.green, Color.blue, Color.yellow),
    defaultTimeout = 10 millis,
    msgDelay = 300 millis
  ){
    app =>

    lazy val visual = new JUNGVisualizationAppExtra(graph, naming, env)
  }

  app.start()
}