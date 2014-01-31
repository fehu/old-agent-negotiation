package feh.tec.agents.coloring.visual.test

import feh.tec.agents.visualisation.Salamander
import feh.util._
import scala.swing.{MainFrame, Frame, GridPanel}
import scala.swing.Swing._
import feh.util.FileUtils
import scala.swing.event.{MouseClicked, MouseMoved}
import com.kitfox.svg.{Polygon, SVGElement}
import scala.collection.JavaConversions._

object ColoringSalamanderTest extends App with FileUtils{
//  val vis = new GenericGraphColoringVisualisation(30, .1, FdpDsl.indent._4)
  val filename = "tmp/graph-coloring-dot"
  val svg = file(filename+".svg") withInputStream File.read[String]

  val panel = Salamander.panel("test", svg.get)
  val d = Salamander.diagram(panel)

  def picked(l: List[List[SVGElement]]){
    l foreach println
    l map (_ map{
      case p =>
        println(
          "getInlineAttributes = " + p.getInlineAttributes + "; " +
          "getPresentationAttributes = " + p.getPresentationAttributes + "; " +
          "id = " + p.getId + "; " +
          "getXMLBase = " + p.getXMLBase
        )
    })
  }

  panel.listenTo(panel.mouse.clicks)
  panel.reactions +={
//    case MouseMoved(_, point, mod) =>
    case MouseClicked(_, point, mod, n, _) =>
      d.pick(point, null)
        .toList.map(_.asInstanceOf[java.util.ArrayList[_]].toList)
        .asInstanceOf[List[List[SVGElement]]]
        .pipe(picked)
  }
  
  val gPanel = new GridPanel(1, 1){
    contents += panel
  }
  val frame = new MainFrame(){
    contents = gPanel
    size = 600 -> 600
  }

  frame.open()
}