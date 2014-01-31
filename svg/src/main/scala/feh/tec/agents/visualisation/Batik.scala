package feh.tec.agents.visualisation

import scala.swing.{Component, Panel}
import org.apache.batik.swing.JSVGCanvas

trait Batik {

}

object SvgCanvas{
  implicit def canvasWrapper(c: SvgCanvas) = c.peer
}

class SvgCanvas(underlying: JSVGCanvas) extends Component{
  def this() = this(new JSVGCanvas())
  override lazy val peer = underlying
}