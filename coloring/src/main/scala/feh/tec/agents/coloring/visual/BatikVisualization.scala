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
import feh.dsl.graphviz.{Prog, DotDsl}
import scala.swing.Swing._
import feh.dsl.graphviz.Prog.Dot
import org.apache.batik.dom.svg.{SVGOMAElement, SVGOMGElement}
import org.w3c.dom.{NodeList, Node}
import org.apache.batik.swing.svg.JSVGComponent
import org.apache.batik.swing.JSVGCanvas
import scala.swing.GridBagPanel.Fill
import akka.actor.{ActorRef, Props, ActorSystem, Actor}
import feh.tec.agents.coloring.visual.ColorUpdateActor.SetColor
import scala.concurrent.duration._
import feh.tec.agents.coloring.util.Name
import feh.tec.agents.coloring.visual.ColorUpdateActor.SetColor
import akka.util.Timeout

class BatikVisualization(val doc: SVGDocument, val elems: Map[UUID, SVGElement]) 
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

  protected var uuidAbove = Option.empty[UUID]

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
    .affect(_.preferredSize = 100 -> 20, _.horizontalAlignment = Alignment.Center)
    .layout(_.fill = Fill.Both)


  val layout = List(
    place(canvas, "canvas").transform(_.addLayout(_.weightx = 1, _.weighty = .9, _.fill = Fill.Both)) in theCenter,
    place(idText, "idText") to theNorth of "canvas"
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
    canvas.getUpdateManager.getUpdateRunnableQueue.invokeLater(Runnable{
      canvas setDocument doc
    })
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
          println("color= " + color)
          color.map(c => node.setAttributeNS(null, "fill", c.hexRGB))
//            .getOrElse( node.setAttributeNS(null, "fill", defaultColor.hexRGB) )
            .getOrElse( node.removeAttributeNS(null, "fill") )
        }
    }

  enableOpts()
  canvas.setSVGDocument(doc)
  canvas.setDocumentState(JSVGComponent.ALWAYS_DYNAMIC)

  println("dyn = " + canvas.isDynamic)
}

object ColorUpdateActor{
  case class SetColor(id: UUID, color: Option[Color])
}

trait GraphColoringApplication extends GraphColoringVisualisation{
  implicit def prog: Prog

  class SvgBuilder(factory: GraphvizGraphFactory with SvgLoader, filename: String){
//    protected  val factory = getFactory(gr)
    factory.writeToFile(factory.Path(filename))
    val doc = factory.load(factory.Path(filename))
    def names = factory.reverseNaming
  }

  protected def factory: GraphvizGraphFactory with SvgLoader
  def dotFilename = "GraphColoringApplication.dot"
  private lazy val svgB = new SvgBuilder(factory, dotFilename)
  lazy val doc = svgB.doc
  lazy val elementByUUID = svgB.names.mapValues{
    name => svgB.doc.getElementById(name).asInstanceOf[SVGElement]
  }

  println("elements = " + elementByUUID)

  protected lazy val vis = new BatikVisualization(doc, elementByUUID)

  def update(id: UUID, color: Option[Color]) = vis.updateColor(elementByUUID(id), color)

  def start() = vis.start()
  def stop() = vis.stop()
}

class ColorUpdateActor(app: GraphColoringApplication) extends Actor{
  def receive = {
    case SetColor(id, colorOpt) => app.update(id, colorOpt)
  }
}

class BatikColoringOverseer(id: UUID, env: ColoringEnvironment, updater: ActorRef) extends ColoringOverseer(id, env){
  override def receive: Actor.Receive = {
    case SetColor(nodeId, color) =>
      env.setColor(nodeId, color)
      updater ! ColorUpdateActor.SetColor(nodeId, color)
  }
}

object GraphColoringApp extends GraphColoringApplication with App{
  implicit def prog = Dot

  lazy val nNodes = 30

  protected lazy val graph = generator.generate("coloring", nNodes, _.nextDouble() < .1)
  protected def generator = new GraphGeneratorImpl
  protected lazy val factory = new GenericGraphvizFactory(DotDsl.indent._4, graph) with SvgLoader
  lazy val ids = elementByUUID.keys.toList
  implicit def reverseNaming = factory.reverseNaming
  
  implicit val system = ActorSystem.create()
  implicit val timeout: Timeout = 1 second span
  lazy val colors = Set(Color.red, Color.blue, Color.green)
  lazy val updaterRef = system.actorOf(Props(classOf[ColorUpdateActor], this))
  lazy val env = new GraphColoring(colors, graph){
    override def overseerProps = Props(classOf[BatikColoringOverseer], overseerId, env, updaterRef)
  }

  def getNeighbours(of: UUID) = graph.neighbouringNodes(of).get.map(n => reverseNaming(n.id))

  def createAgent = env.createAgent(reverseNaming, getNeighbours) _

  lazy val agentRefs = ids.map(createAgent)

//  scheduler.schedule(1 second, 1 second){
//    update(ids.randomChoice, Some(Color.blue))
//  }

  override def start() = {
    agentRefs.foreach(ColoringAgent.start)
    vis.size = 800 -> 800
    super.start()
  }

  AgentReport.default.enabled = true
  start()

}

