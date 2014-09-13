package feh.tec.web

import org.scalajs.dom.{MessageEvent, WebSocket}

import scala.scalajs.js

trait SocketConnections {
  protected def sockets: Seq[WebSocket]

  protected def setSocketsCallbacks() = sockets foreach{
    s => s.onmessage = {
      case ev: MessageEvent => onMessage(ev.data)
    }: PartialFunction[MessageEvent, Unit]
  }
  def onMessage: PartialFunction[js.Any, Unit]

  setSocketsCallbacks()
}
