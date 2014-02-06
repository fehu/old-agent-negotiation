package feh.tec.agents.coloring.visual

import feh.tec.agents.comm.coloring._
import scala.swing.{Panel, MainFrame}
import feh.dsl.swing.{SwingFrameAppCreation, SwingAppFrame}
import java.util.UUID
import java.awt.{Paint, Color}
import feh.util._
import edu.uci.ics.jung.graph.{UndirectedSparseGraph, Graph}
import feh.tec.agents.coloring.util.{NameGenerator, Name}
import edu.uci.ics.jung.algorithms.layout._
import edu.uci.ics.jung.visualization.{VisualizationViewer, BasicVisualizationServer}
import akka.actor.ActorSystem
import akka.util.Timeout
import scala.concurrent.duration._
import scala.swing.GridBagPanel.Fill
import org.apache.commons.collections15.Transformer
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import scala.collection.mutable
import scala.swing.Swing._
import edu.uci.ics.jung.visualization.control._
import java.awt.event._
import feh.tec.agents.coloring.visual.JUNGBaseVisualization.{SelectionEvents, MouseManipulations}
import scala.xml.Xhtml
import akka.actor.ActorDSL._

class JUNGBaseVisualization(val graph: Graph[Name, (Name, Name)],
                        bLayout: Graph[Name, (Name, Name)] => AbstractLayout[Name, (Name, Name)])
                       (gr: ColoringGraph, 
                        val naming: UUID => Name)
  extends MainFrame with SwingAppFrame with SwingFrameAppCreation.Frame9PositionsLayoutBuilder{

  protected def putNodes() = gr.nodes foreach{ n => graph.addVertex(naming(n.id)) }
  protected def putEdges() = gr.edges foreach{
    case (i1, i2) =>
      val n1 = naming(i1)
      val n2 = naming(i2)
      graph.addEdge(n1 -> n2, n1, n2)
  }

  lazy val graphLayout = bLayout(graph)
  lazy val vv = new VisualizationViewer(graphLayout)

  protected val defaultVertexColor = Color.white
  protected val vertexColors = mutable.Map(gr.nodes.toSeq.flatMap(n => n.value.map(naming(n.id) -> _)): _*)
    .withDefault(_ => defaultVertexColor)
  
  lazy val vertexPaint = new Transformer[Name, Paint] {
    def transform(p1: Name) = vertexColors(p1)
  }

  def updateColor(vertex: Name, color: Option[Color]) = {
    color match{
      case Some(c) => vertexColors += vertex -> c
      case None => vertexColors -= vertex
    }
    vv.repaint()
  }

  def setupFeatures(){
    vv.getRenderContext.setVertexFillPaintTransformer(vertexPaint)
    vv.getRenderContext.setVertexLabelTransformer(new ToStringLabeller)
    vv.getRenderer.getVertexLabelRenderer.setPosition(Position.CNTR)
  }

  def start() = {
    putNodes()
    putEdges()
    setupFeatures()

    open()
    size = 800 -> 800
    buildLayout()
  }
  def stop() = close()

  lazy val visPanel = new Panel { peer.add(vv) }

  protected def appLayout = List(
    place(visPanel, "vv")
      .transform(_.addLayout(_.fill = Fill.Both)) in theCenter
  ) 
  
  lazy val layout = appLayout 
}

object JUNGBaseVisualization{
  
  /** Enables some JUNG mouse features:
   * selecting (and moving) with left button, adding to selection using shift,
   * translating with right button
   * zooming with scroll
   */
  trait MouseManipulations{
    self: JUNGBaseVisualization =>

    val gm = new PluggableGraphMouse
    gm.add(new TranslatingGraphMousePlugin(InputEvent.BUTTON3_MASK))
    gm.add(new ScalingGraphMousePlugin(new CrossoverScalingControl(), 0, 1.1f, 0.9f))

    gm.add(new PickingGraphMousePlugin(InputEvent.BUTTON1_MASK, InputEvent.BUTTON1_MASK + InputEvent.SHIFT_MASK) )

    vv.setGraphMouse(gm)
  }

  trait SelectionEvents{
    self: JUNGBaseVisualization =>

    protected implicit def system: ActorSystem
    protected def scheduler = system.scheduler
    protected implicit def execContext = system.dispatcher

    protected val _onVertexSelection = mutable.Buffer.empty[Set[Name] => Unit]
    def onVertexSelection(f: Set[Name] => Unit) = _onVertexSelection += f
    def onVertexSelection = _onVertexSelection.toSeq

    protected var _pickedNames = Set.empty[Name]
    protected def pickedNames = _pickedNames.toSet

    protected val pickedState = vv.getPickedVertexState
    pickedState.addItemListener(new ItemListener {
      def itemStateChanged(e: ItemEvent) = {
        val name = e.getItem.asInstanceOf[Name]
        if(pickedState.isPicked(name)) pickedNamesSettingActor ! name
      }

    })

    protected def setPickedNames(names: Set[Name]) = {
      _pickedNames = names
      onVertexSelection foreach (_(pickedNames))
    }
    
    // in nanos
    protected val namesPickingDelay = 100
    protected val namesPickingTickDelay = 100 nanos span
    protected lazy val pickedNamesSettingActor = actor(new Act {
      var lastMessage: Long = -1
      val buff = mutable.Buffer.empty[Name]

      case object Tick

      become{
        case name: Name =>
          buff += name
          lastMessage = System.nanoTime()
        case Tick =>
          if(lastMessage != -1 && System.nanoTime() - lastMessage > namesPickingDelay) {
            setPickedNames(buff.toSet)
            buff.clear()
            lastMessage = -1
          }
      }

      scheduler.schedule(0 nanos, namesPickingTickDelay, self, Tick)
    })


  }
  
}

trait JUNGInfoVisualization extends JUNGBaseVisualization with SelectionEvents{
  protected var selectedVertex: Option[Name] = None

  def infoExtractor: OneTryColoringStateInfoExtractor

  val nodeInfo = monitorFor(selectedVertex).text
    .extractFuture(infoExtractor.nodeAgentInfo _ andThen (_.map(Xhtml.toXhtml)))
    .affect(_.preferredSize = 100 -> 500)
    .layout(_.fill = Fill.Both, _.weightx = 1.0/3, _.weighty = .5)

  override protected def appLayout = super.appLayout ::: List(
    place(scrollable()(nodeInfo, "info")) to theWest of "vv"
  )

  
  
  
  onVertexSelection{names =>
    selectedVertex = if(names.size == 1) Some(names.head) else None
    updateForms()
  }
}



class JUNGVisualizationApp(val graph: ColoringGraph,
                           naming: UUID => Name) extends GraphColoringVisualisation{

  lazy val vis = new JUNGBaseVisualization(new UndirectedSparseGraph, new KKLayout(_))(graph, naming)

  def update(name: Name, color: Option[Color]) = { vis.updateColor(name, color)}
  def update(id: UUID, color: Option[Color]) = update(naming(id), color)

  def start() = vis.start()
  def stop() = vis.stop()

  def frame = vis
  type Visualization = BasicVisualizationServer[Name, (Name, Name)]
  def graphVisualization = vis.vv
}



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
    visual.frame.size = 800 -> 800
    println("starting: " + starting)
    ColoringAgent start starting.actor
  }
}

object JUNGVisualizationApp extends App{
  val app = new ColoringVisualizationApplication(
    graph = ColoringGraphGenerator.generate("coloring", 30, _.nextDouble() < .1),
    colors = Set(Color.red, Color.green, Color.blue, Color.yellow),
    defaultTimeout = 10 millis,
    msgDelay = 300 millis
  ){
    app =>

    lazy val visual = new JUNGVisualizationApp(graph, naming) {

      override lazy val vis = new JUNGBaseVisualization(new UndirectedSparseGraph, new KKLayout(_))(graph, naming)
        with MouseManipulations with JUNGInfoVisualization
      {
        protected implicit def system = app.system
        lazy val infoExtractor = new OneTryColoringStateInfoExtractor(env, 10 millis)
      }

    }
  }

  app.start()
}