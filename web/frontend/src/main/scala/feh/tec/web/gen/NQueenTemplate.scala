package feh.tec.web.gen

import feh.tec.web.NQueen
import feh.tec.web.common.WebsocketConf
import feh.tec.web.util.GenTemplate
import feh.util.Path.EmptyPath
import feh.util.PathSelector
import feh.util.file._
import scala.xml.NodeSeq
import org.scalajs.jquery._

class NQueenTemplate extends GenTemplate("n-queen", GenTemplate.classOf(NQueen)) with WebsocketConf{
  override def templateBody: NodeSeq =
      <div containerFor="chess-board"/> :: <div containerFor="queen-info"/> :: Nil


  override def templateHead: NodeSeq = <meta ws={readWsConfig()} /> :: Nil

  override def css: PathSelector = PathSelector(".")
    .select("n-queen".p / *)
    .select("tablesorter".p /? (_.endsWith(".css")))
    .select("bootstrap" / "bootstrap.css")
    .select("tablesorter" / "pager" / "jquery.tablesorter.pager.css")


  override def includeStyleResources = EmptyPath.Relative
    .select("bootstrap" / "fonts" / *)

  protected def readWsConfig(): String = wsConf.front.url("n-queen")

}
