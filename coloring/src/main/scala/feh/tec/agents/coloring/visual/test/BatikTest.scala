package feh.tec.agents.coloring.visual.test

import feh.util.FileUtils
import feh.tec.agents.visualisation.SvgCanvas
import org.apache.batik.util.XMLResourceDescriptor
import org.apache.batik.dom.svg.SAXSVGDocumentFactory
import java.io.StringReader
import scala.swing.{Component, MainFrame, GridPanel}
import scala.swing.Swing._
import org.w3c.dom.Node
import scala.swing.event.MouseClicked
import org.w3c.dom.svg.SVGElement
import org.w3c.dom.events.{Event, EventListener, EventTarget}

object BatikTest extends App with FileUtils{
  val filename = "tmp/graph-coloring-dot"
  val svg = file(filename+".svg") withInputStream File.read[String]

  val doc = new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName)
    .createSVGDocument(null, new StringReader(svg.get))

  val c = new SvgCanvas()

  c.setEnableImageZoomInteractor(true)
  c.setEnablePanInteractor(true)
  c.setEnableResetTransformInteractor(true)
  c.setEnableRotateInteractor(true)
  c.setEnableZoomInteractor(true)

  c.setSVGDocument(doc)

  val m = doc.getElementById("m").asInstanceOf[SVGElement]
  println("m=" + m)

  m.setAttribute("fill", "red")

  val b = List.newBuilder[Node]
  val attrs = m.getAttributes
  for(i <- 0 to attrs.getLength) Option(attrs.item(i)) foreach (b +=)
  val an = b.result()
  println("an = " + an)
  println("m attrs: " + an.map(attr => attr.getNodeName + "=" + attr.getNodeValue).mkString(", "))

  m.asInstanceOf[EventTarget].addEventListener("mouseover", new EventListener {
    def handleEvent(evt: Event) = println(evt)
  }, false)

//  c.listenTo(c.mouse.clicks)
  c.reactions += {
    case MouseClicked(s, point, mod, n, _) =>
  }

  val p = new GridPanel(1, 1){
    contents += c
  }

  val frame = new MainFrame{
    contents = p
    size = 600 -> 600
  }

  frame.open()
}

