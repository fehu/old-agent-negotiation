package feh.tec.web

import org.scalajs.dom.{MessageEvent, WebSocket}

trait SocketConnections {
  protected def sockets: Seq[WebSocket]

  protected def setSocketsCallbacks() = sockets foreach{
    s => s.onmessage = {
      case ev: MessageEvent => onMessage(ev.data)
    }: PartialFunction[MessageEvent, Unit]
  }
  def onMessage: PartialFunction[Any, Unit]

  setSocketsCallbacks()
}
