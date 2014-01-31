package feh.tec.agents.visualisation

import com.kitfox.svg.app.beans.{SVGPanel, SVGIcon}
import com.kitfox.svg.{SVGDiagram, SVGCache}
import java.io.StringReader
import feh.util._
import java.net.URI
import scala.swing.Panel

trait Salamander {
  protected def create[T <: {def setAntiAlias(b: Boolean); def setSvgURI(uri: URI); def setScaleToFit(b: Boolean)}]
    (name: String, svg: String, elem: T) =
    SVGCache.getSVGUniverse.loadSVG(new StringReader(svg), name) |> {
      uri =>
        elem.setAntiAlias(true)
        elem.setSvgURI(uri)
        elem.setScaleToFit(true)
        elem
    }

  def icon(name: String, svg: String): SVGIcon = create(name, svg, new SVGIcon)

  def panel(name: String, svg: String): ScalaSVGPanel = new ScalaSVGPanel(create(name, svg, new SVGPanel))

  def diagram(uri: URI): SVGDiagram = SVGCache.getSVGUniverse.getDiagram(uri)
  def diagram(panel: SVGPanel): SVGDiagram = SVGCache.getSVGUniverse.getDiagram(panel.getSvgURI)
}

object Salamander extends Salamander

object ScalaSVGPanel{
  implicit def scalaSVGPanelToPanel(u: ScalaSVGPanel) = u.peer
}

class ScalaSVGPanel(underlying: SVGPanel) extends Panel{
  override lazy val peer = underlying
}