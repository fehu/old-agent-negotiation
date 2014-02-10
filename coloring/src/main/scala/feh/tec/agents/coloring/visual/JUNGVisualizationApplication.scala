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
import feh.tec.agents.comm.coloring.ColoringGraphGenerator.RandConfig

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

class JUNGVisualizationAppExtra(graph: ColoringGraph, env: GraphColoring)
                               (implicit asystem: ActorSystem)
  extends JUNGVisualizationApp(graph, env.naming) with JUNGVisualizationBuilderExtraLayoutImpl
{
  override lazy val vis = new JUNGBaseVisualization(new UndirectedSparseGraph, null)(graph, env.naming)
    with JUNGVisualizationFeatures with JUNGInfoVisualization
  {
    lazy val infoExtractor = new OneTryColoringStateInfoExtractor(env, 10 millis)
    protected implicit def system = asystem
    def overseer = env.overseer
    implicit def tBuilder = str =>
      if(str == "") Some(Name(""))
      else env.naming.values.find(_.name == str)
  }

}

object JUNGVisualizationApplication extends App{
  val app = new ColoringVisualizationApplication(
    graph = ColoringGraphGenerator.generate("coloring", 30, _.nextDouble() < .1, RandConfig(Some(4))),
    colors = Set(Color.red, Color.green, Color.blue, Color.yellow),
    defaultTimeout = 10 millis,
    tickDelay = 200 millis,
    msgDelay = 1 milli //300 millis
  ){
    app =>

    lazy val visual = new JUNGVisualizationAppExtra(graph, env)
  }

  app.start()
}