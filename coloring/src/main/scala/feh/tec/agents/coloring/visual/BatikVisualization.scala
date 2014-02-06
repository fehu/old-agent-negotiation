package feh.tec.agents.coloring.visual

import feh.util._
import org.w3c.dom.svg.{SVGElement, SVGDocument}
import feh.tec.agents.visualisation.SvgCanvas
import java.util.UUID
import feh.tec.agents.comm.coloring._
import scala.swing._
import feh.dsl.swing.{SwingFrameAppCreation, SwingAppFrame}
import java.awt.Color
import org.w3c.dom.events.{Event, EventListener, EventTarget}
import feh.dsl.graphviz.{FdpDsl, Prog}
import scala.swing.Swing._
import feh.dsl.graphviz.Prog._
import org.w3c.dom.{NodeList, Node}
import org.apache.batik.swing.svg.JSVGComponent
import scala.swing.GridBagPanel.Fill
import akka.actor.ActorSystem
import scala.concurrent.duration._
import akka.util.Timeout
import scala.xml.{Xhtml, NodeSeq}
import feh.tec.agents.comm.coloring.OneTryColoring.{StateInfo, GetStateInfo}
import scala.concurrent.{ExecutionContext, Future}
import feh.tec.agents.coloring.util.Name

class BatikVisualization(val doc: SVGDocument, val elems: Map[UUID, SVGElement],
                         mouseOverMsg: Option[UUID] => String, extractNodeInfo: Option[UUID] => Future[NodeSeq])
                        (implicit execContext: ExecutionContext)
  extends MainFrame with SwingAppFrame with SwingFrameAppCreation.Frame9PositionsLayoutBuilder
{
  val canvas = new SvgCanvas()
  
  def enableOpts(){
    canvas.setEnableImageZoomInteractor(true)
    canvas.setEnablePanInteractor(true)
    canvas.setEnableResetTransformInteractor(true)
    canvas.setEnableRotateInteractor(true)
    canvas.setEnableZoomInteractor(true)
    canvas.setAutoscrolls(true)
  }

  def start() = {
    buildLayout()
    open()
    println(canvas.showing)
  }

  def stop() = close()

  elems.withFilter(_._2 != null).foreach{
    case (id, elem) =>
      val el = elem.asInstanceOf[EventTarget]

      el.addEventListener("mouseover", new EventListener {
        def handleEvent(evt: Event) = elements.event.mouse.mouseOver.foreach(_(id))
      }, false)
      el.addEventListener("mouseout", new EventListener {
        def handleEvent(p1: Event) = elements.event.mouse.mouseOut.foreach(_(id))
      }, false)
  }


  object elements{
    object event{
      object mouse{
        protected [BatikVisualization] var mouseOver: Seq[UUID => Unit] = Nil
        def over(f: UUID => Unit){ mouseOver :+= f }

        protected [BatikVisualization] var mouseOut: Seq[UUID => Unit] = Nil
        def out(f: UUID => Unit){ mouseOut :+= f }
      }

    }
  }

  protected var _selectedId: UUID = null
  def selectedId = Option(_selectedId)

  elements.event.mouse.over{ id =>
    _selectedId = id
    updateForms()
  }
  elements.event.mouse.out{ _ =>
    _selectedId = null
    updateForms()
  }

  val idText = monitorFor(_selectedId).asTextField
    .extract(mouseOverMsg compose Option.apply)
    .affect(_.preferredSize = 100 -> 20, _.horizontalAlignment = Alignment.Center)
    .layout(_.fill = Fill.Both)

  val nodeInfo = monitorFor(_selectedId).text
    .extractFuture(extractNodeInfo andThen (_.map(Xhtml.toXhtml)) compose Option.apply)
    .affect(_.preferredSize = 100 -> 500)
    .layout(_.fill = Fill.Both, _.weightx = 1.0/3, _.weighty = .5)

  val layout = List(
    place(canvas, "canvas").transform(_.addLayout(_.weightx = 1, _.weighty = .9, _.fill = Fill.Both)) in theCenter,
    place(idText, "idText") to theNorth of "canvas",
    place(scrollable()(nodeInfo, "info")) to theWest of "canvas"
  )

  implicit class NodeAttrsWrapper(node: Node){
    def attrs = Option(node.getAttributes).map{
      a =>
        (for(i <- 0 until a.getLength) yield a.item(i) |> {
          node => node.getNodeName -> node.getNodeValue
        }).toMap
    } getOrElse Map()
  }

  def updating[R](f: => R): R = {
    val r = f
    val um = canvas.getUpdateManager
//    um.getUpdateRunnableQueue.invokeLater(Runnable{ // todo
      canvas setDocument doc
//    })
    r
  }

  protected def findElementChildren(el: SVGElement) = el.getChildNodes.toList.collect{case el: SVGElement => el}
  protected def findElementGrandChildren(el: SVGElement) =
    findElementChildren(el) flatMap findElementChildren collect {case el: SVGElement => el}

  implicit class NodeListWrapper(nl: NodeList){
    def toList = {
      val b = List.newBuilder[Node]
      for(i <- 0 until nl.getLength) b += nl.item(i)
      b.result()
    }
  }

  def updateColor(el: SVGElement, color: Option[Color]) =
    findElementGrandChildren(el).withFilter(e => e.getNodeName == "ellipse" || e.getNodeName == "polygon") map {
      node =>
        updating{
//          println("color= " + color)
          color.map(c => node.setAttributeNS(null, "fill", feh.util.color.names get c getOrElse c.hexRGB))
//            .getOrElse( node.setAttributeNS(null, "fill", defaultColor.hexRGB) )
            .getOrElse( node.removeAttributeNS(null, "fill") )
        }
    }

  enableOpts()
  canvas.setSVGDocument(doc)
  canvas.setDocumentState(JSVGComponent.ALWAYS_DYNAMIC)
}


trait GraphvizGraphColoringApplication extends GraphColoringVisualisation{
  implicit def prog: Prog
  implicit def execContext: ExecutionContext

  class SvgBuilder(factory: GraphvizGraphFactory with SvgLoader, filename: String){
    factory.writeToFile(factory.Path(filename))
    val doc = factory.load(factory.Path(filename))
    def names = factory.naming
  }

  protected def factory: GraphvizGraphFactory with SvgLoader
  def dotFilename = "GraphColoringApplication.dot"
  private lazy val svgB = new SvgBuilder(factory, dotFilename)
  lazy val doc = svgB.doc
  lazy val elementByUUID = svgB.names.mapValues{
    name => svgB.doc.getElementById(name).asInstanceOf[SVGElement]
  }

  def env: GraphColoring

  println("elements = " + elementByUUID)

  def nodeBasicInfo(idOpt: Option[UUID]) = idOpt map {
    id =>
      val name = factory.naming(id)
      val neighbours = graph.neighbouringNodes(id).map(factory naming _.id)
      s"$name($id), neighbours: ${neighbours.mkString(", ")}"
  } getOrElse ""

  def nodeUpdateResponseTimeout: Timeout = 10 millis span

  def nodeAgentInfo(idOpt: Option[UUID]): Future[NodeSeq] = idOpt map {
    id => env.agentController.askAny(svgB.names(id), GetStateInfo)(nodeUpdateResponseTimeout) map {
      case StateInfo(name, isActive, color, tries, proposal, acceptance, freeColors, neighboursColors) =>
        val (accepted, pending) = acceptance.span(_._2)
        <html>
          <table>
            <tr><td>name</td><td>{name.name}</td></tr>
            <tr><td>isActive</td><td>{isActive.toString}</td></tr>
            <tr><td>color</td><td>{color.map(_.stringRGB) getOrElse "None"}</td></tr>
            <tr><td>tries</td><td>{tries.map(_.stringRGB).mkString(", ")}</td></tr>
            <tr><td>proposal</td><td>{Option(proposal).map(_.stringRGB) getOrElse "None"}</td></tr>
            <tr><td>accepted</td><td>{accepted.keys.mkString(", ")}</td></tr>
            <tr><td>pending</td><td>{pending.keys.mkString(", ")}</td></tr>
            <tr><td>freeColors</td><td>{freeColors.map(_.stringRGB).mkString(", ")}</td></tr>
            <tr><td>neighbours</td><td>
              <table>
              {
                neighboursColors.map{case (n, cOpt) => <tr><td>{n.name}</td><td>{cOpt.map(_.stringRGB) getOrElse "None"}</td></tr>}
              }
              </table></td></tr>
          </table>
        </html>
    }
//<tr><td></td><td>{}</td></tr>
  } getOrElse Future.successful(<html>No info</html>)

  protected lazy val vis = new BatikVisualization(doc, elementByUUID, nodeBasicInfo, nodeAgentInfo)

  def update(id: UUID, color: Option[Color]) = vis.updateColor(elementByUUID(id), color)
  def update(name: Name, color: Option[Color]) = update(factory.reverseNaming(name), color)

  def start() = vis.start()
  def stop() = vis.stop()
}

object GraphvizGraphColoringApp$ extends GraphvizGraphColoringApplication with App{
  implicit def prog = Fdp

  lazy val nNodes = 30
//  lazy val agentActivePhaseDelay = 200 millis span

  protected lazy val graph = generator.generate("coloring", nNodes, _.nextDouble() < .1)
  protected def generator = new ColoringGraphGeneratorImpl
  protected lazy val factory = new GenericGraphvizFactory(FdpDsl.indent._4, graph) with SvgLoader
  lazy val ids = elementByUUID.keys.toList
  implicit def reverseNaming = factory.naming
  
  implicit val system = ActorSystem.create()
  implicit def execContext = system.dispatcher
  implicit val timeout: Timeout = 1 second span
  lazy val colors = Set(Color.red, Color.blue, Color.green, Color.yellow)
  lazy val updaterRef = ColorUpdateActor.actor(this)
  lazy val env = new GraphColoring(colors, graph /*agentActivePhaseDelay, system.scheduler*/){
    override protected def buildOverseer = new ColoringUpdateOverseer(env, updaterRef)
  }


  def frame = vis
  type Visualization = SvgCanvas
  def graphVisualization = vis.canvas

  def msgDelay = 400 millis span
  def getNeighbours(of: UUID) = graph.neighbouringNodes(of).map(n => reverseNaming(n.id))
  def createAgent = env.createAgent(reverseNaming, getNeighbours, env.buildRef, msgDelay) _

  lazy val agents = ids.map(createAgent)

  val starting = agents.randomChoice
  println("starting: " + starting)

  override def start() = {
//    agents.map(_.actor).foreach(ColoringAgent.start)
    super.start()
    vis.size = 800 -> 800
    ColoringAgent start starting.actor
  }

  AgentReport.default.enabled = true
  start()

}

