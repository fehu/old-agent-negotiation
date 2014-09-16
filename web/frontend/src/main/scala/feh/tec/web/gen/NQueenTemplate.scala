package feh.tec.web.gen

import feh.tec.web.NQueen
import feh.tec.web.common.WebsocketConf
import feh.tec.web.util.GenTemplate
import feh.util.PathSelector
import feh.util.file._
import scala.scalajs.js
import scala.xml.NodeSeq
import org.scalajs.jquery._

class NQueenTemplate extends GenTemplate("n-queen", GenTemplate.classOf(NQueen)) with WebsocketConf{
  override def templateBody: NodeSeq =
      <div containerFor="chess-board"/> :: <div containerFor="queen-info"/> :: Nil

  override def templateHead: NodeSeq = <meta ws={readWsConfig()} /> :: Nil

  override def css: PathSelector = PathSelector(".")
    .select("n-queen".p.selectAll())
    .select("tablesorter".p.select(_.endsWith(".css")))

  protected def readWsConfig(): String = wsConf.front.url("n-queen")

}
