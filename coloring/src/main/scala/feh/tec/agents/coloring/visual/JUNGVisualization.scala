package feh.tec.agents.coloring.visual

import feh.tec.agents.comm.coloring._
import scala.swing._
import feh.dsl.swing.SwingAppBuildingEnvironment
import java.util.UUID
import java.awt.{Paint, Color}
import edu.uci.ics.jung.graph.Graph
import feh.tec.agents.coloring.util.Name
import edu.uci.ics.jung.algorithms.layout._
import edu.uci.ics.jung.visualization.VisualizationViewer
import akka.actor.ActorSystem
import scala.concurrent.duration._
import scala.swing.GridBagPanel.Fill
import org.apache.commons.collections15.Transformer
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position
import scala.collection.mutable
import edu.uci.ics.jung.visualization.control._
import java.awt.event._
import scala.xml.Xhtml
import akka.actor.ActorDSL._
import feh.util._
import scala.swing.ScrollPane.BarPolicy
import scala.Some

trait JUNGVisualizationBuilder extends SwingAppBuildingEnvironment{
  class JUNGBaseVisualization(val graph: Graph[Name, (Name, Name)],
                              bLayout: Graph[Name, (Name, Name)] => AbstractLayout[Name, (Name, Name)])
                             (gr: ColoringGraph,
                              val naming: UUID => Name)
    extends MainFrame with SwingAppFrame with Frame9PositionsLayoutBuilder{

    putNodes()
    putEdges()

    protected def putNodes() = gr.nodes foreach{ n => graph.addVertex(naming(n.id)) }
    protected def putEdges() = gr.edges foreach{
      case (i1, i2) =>
        val n1 = naming(i1)
        val n2 = naming(i2)
        graph.addEdge(n1 -> n2, n1, n2)
    }

    def graphLayout = {
      bLayout(graph)
    }
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
      setupFeatures()

      open()
      size = 800 -> 800
      buildLayout()
    }
    def stop() = close()

    lazy val visPanel = new Panel { peer.add(vv) }

    protected def appLayout: List[AbstractLayoutSetting] = List(
      place(visPanel, "vv")
        .transform(_.addLayout(_.fill = Fill.Both, _.weightx = .9, _.weighty = .9)) in theCenter
    )

    lazy val layout = appLayout
  }

}


trait JUNGVisualizationBuilderExtra{
  self: JUNGVisualizationBuilder =>
  
  /** Enables some JUNG mouse features:
   * selecting (and moving) with left button, adding to selection using shift,
   * translating with right button
   * zooming with scroll
   */
  trait MouseManipulations extends JUNGBaseVisualization{

    lazy val gm = new PluggableGraphMouse

    abstract override def setupFeatures(){
      super.setupFeatures()
      gm.add(new TranslatingGraphMousePlugin(InputEvent.BUTTON3_MASK))
      gm.add(new ScalingGraphMousePlugin(new CrossoverScalingControl(), 0, 1.1f, 0.9f))
      gm.add(new PickingGraphMousePlugin(InputEvent.BUTTON1_MASK, InputEvent.BUTTON1_MASK + InputEvent.SHIFT_MASK) )

      vv.setGraphMouse(gm)
    }
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

  trait LayoutChanger {
    self: JUNGBaseVisualization =>

    lazy val buildGraphLayout: Map[String, Graph[Name, (Name, Name)] => AbstractLayout[Name, (Name, Name)]] = Map(
      "Circle"                -> ((gr: Graph[Name, (Name, Name)]) => new CircleLayout(gr)),
      "Fruchterman-Reingold"  -> ((gr: Graph[Name, (Name, Name)]) => new FRLayout(gr)),
      "Self-organizing map"   -> ((gr: Graph[Name, (Name, Name)]) => new ISOMLayout(gr)),
      "Kamada-Kawai"          -> ((gr: Graph[Name, (Name, Name)]) => new KKLayout(gr)),
      "Spring"                -> ((gr: Graph[Name, (Name, Name)]) => new SpringLayout(gr))
    )
    lazy val graphLayoutNames = buildGraphLayout.keys.toSeq
    protected lazy val buildGraphLayoutCash = mutable.Map.empty[String, AbstractLayout[Name, (Name, Name)]]
    def layoutByName(n: String) = buildGraphLayoutCash.get(n).getOrElse{
      buildGraphLayout(n)(graph) $$ {
        buildGraphLayoutCash += n -> _
      }
    }
    protected var currentGraphLayout = layoutByName(buildGraphLayout.head._1)
    override def graphLayout = currentGraphLayout

    protected def updateGraphVisualization(){
      vv.setGraphLayout(graphLayout)
      vv.doLayout()
    }

    lazy val layoutChooserBuilder = controlForSeq[String](graphLayoutNames, static = true)
      .dropDownList(name => {
        currentGraphLayout = layoutByName(name)
        updateGraphVisualization()
      })
  }
}

trait JUNGVisualizationBuilderExtraLayoutImpl extends JUNGVisualizationBuilderExtra{
  self: JUNGVisualizationBuilder =>

  trait JUNGInfoVisualization extends JUNGBaseVisualization with SelectionEvents
  {
    protected var selectedVertex: Option[Name] = None

    def infoExtractor: OneTryColoringStateInfoExtractor

    val nodeInfo = monitorFor(selectedVertex).text
      .extractFuture(infoExtractor.nodeAgentInfo _ andThen (_.map(Xhtml.toXhtml)))

    onVertexSelection{names =>
      println(s"names=$names")
      selectedVertex = if(names.size == 1) Some(names.head) else None
      updateForms()
    }
  }

  trait JUNGVisualizationFeatures extends JUNGBaseVisualization
    with MouseManipulations with LayoutChanger with JUNGInfoVisualization
  {
    lazy val controlPanelBuilder = panel.gridBag(
      place(scrollable()(nodeInfo, "node-info")
          .fillBoth.yWeight(.9).xWeight(1).insets(10)(top = 0, right = 5)
        ) in theCenter,
      place(layoutChooserBuilder
          .fillBoth.xWeight(1).insets(10)(right = 5),
        "layout-chooser") to theNorth of "node-info"
    ).affect(
      _.border = Swing.LineBorder(Color.red),
      p => println("c = " + p.layout)
      )

    override protected def appLayout = List(
      SplitLayout(Orientation.Vertical,
        controlPanelBuilder -> "control",
        visPanel -> "graph"
      )(
        pos = theCenter
        )
    )
  }
}

