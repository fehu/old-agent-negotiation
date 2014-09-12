package feh.tec.web

import scala.scalajs.js.JSApp
import org.scalajs.jquery.jQuery
import org.scalajs.dom
import org.scalajs.dom.{WebSocket, document}

object NQueen extends JSApp with SocketConnections{

  def host = "ws://localhost:8080"

  protected lazy val sockets = new WebSocket(host) :: Nil

  def onMessage: PartialFunction[Any, Unit] = {
    case x => appendPar(document.body, x.toString)
  }

  def appendPar(target: dom.Node, text: String): Unit ={
    jQuery(target).append("<p>"+text+"</p>")
  }

  def main() = {
    appendPar(document.body, "Starting")
  }
}
