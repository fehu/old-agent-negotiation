package feh.tec.agents.coloring.visual

import feh.util._
import org.w3c.dom.svg.{SVGElement, SVGDocument}
import feh.tec.agents.visualisation.SvgCanvas
import java.util.UUID
import feh.tec.agents.comm.coloring.{GraphGeneratorImpl, ColoringGraph, GraphColoringVisualisation}
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
import akka.actor.{ActorSystem, Actor}
import feh.tec.agents.coloring.visual.ColorUpdateActor.SetColor
import scala.concurrent.duration._

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
        (for(i <- 0 to a.getLength) yield Option(a.item(i)) map {
          node => node.getNodeName -> node.getNodeValue
        }).flatten.toMap
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
      for(i <- 0 to nl.getLength) Option(nl) foreach (l => b += l.item(i))
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
  protected def getFactory: ColoringGraph => GraphvizGraphFactory with SvgLoader

  implicit def prog: Prog

  class SvgBuilder(gr: ColoringGraph){
    protected  val factory = getFactory(gr)
    protected val fn = "GraphColoringApplication"
    factory.writeToFile(factory.Path(fn))
    val doc = factory.load(factory.Path(fn))
    val names = factory.naming
  }

  private val svgB = new SvgBuilder(graph)
  val doc = svgB.doc
  val elements = svgB.names.mapValues{
    name => svgB.doc.getElementById(name).asInstanceOf[SVGElement]
  }

  println("elements = " + elements)

  protected val vis = new BatikVisualization(doc, elements)

  import vis.NodeAttrsWrapper


//  vis.elements.event.mouseOver(id => {
//    println("Z")
//    update(id, Some(Color.red))
//    println("attrs = " + elements(id).attrs)
//  })
//  vis.elements.event.mouseOut(id => {
//    println("A")
//    println("attrs = " + elements(id).attrs)
//    update(id, None)
//  })

  def update(id: UUID, color: Option[Color]) = vis.updateColor(elements(id), color)

  def start() = vis.start()
  def stop() = vis.stop()
}

class ColorUpdateActor(app: GraphColoringApplication) extends Actor{
  def receive = {
    case SetColor(id, colorOpt) => app.update(id, colorOpt)
  }
}

object GraphColoringApp extends GraphColoringApplication with App{
  implicit def prog = Dot

  protected lazy val graph = generator.generate("coloring", 30, _.nextDouble() < .1)
//  println(graph)

  protected def getFactory = gr => new GenericGraphvizFactory(DotDsl.indent._4, gr) with SvgLoader
  protected def generator = new GraphGeneratorImpl

  val ids = elements.keys.toList

  implicit val system = ActorSystem.create()
  import system._

  scheduler.schedule(1 second, 1 second){
    update(ids.randomChoice, Some(Color.blue))
  }

  vis.size = 800 -> 800
  start()
}

