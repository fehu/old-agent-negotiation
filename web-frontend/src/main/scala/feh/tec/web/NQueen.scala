package feh.tec.web

import scala.scalajs.js.JSApp
import org.scalajs.jquery.jQuery
import org.scalajs.dom
import dom.document

object NQueen extends JSApp{
  def main() = {
    appendPar(document.body, "AAA")
  }

  def appendPar(target: dom.Node, text: String): Unit ={
    jQuery(target).append("<p>"+text+"</p>")
  }
}
