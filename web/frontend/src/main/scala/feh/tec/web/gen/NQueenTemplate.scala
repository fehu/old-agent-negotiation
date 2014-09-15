package feh.tec.web.gen

import feh.tec.web.NQueen
import feh.tec.web.common.WebsocketConf
import feh.tec.web.util.GenTemplate
import feh.util.PathSelector
import feh.util.file._
import scala.xml.NodeSeq

class NQueenTemplate extends GenTemplate("n-queen", GenTemplate.classOf(NQueen)) with WebsocketConf{
  override def templateBody: NodeSeq =
      <div containerFor="chess-board"/> :: <div containerFor="queen-info"/> :: Nil

  override def templateHead: NodeSeq = <meta ws={readWsConfig()} ></meta>

  override def css: PathSelector = "n-queen".p.selectAll()

  protected def readWsConfig(): String = wsConf.front.url("n-queen")

}
