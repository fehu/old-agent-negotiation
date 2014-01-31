package feh.tec.agents.coloring.visual.test

import feh.util._
import feh.dsl.graphviz._
import feh.dsl.graphviz.Prog._
import feh.dsl.graphviz.OutFormat._
import feh.tec.agents.coloring.visual.GenericAutoGraphvizFactory

class TestDotDsl(_defaultIndent: Int) extends DotDslImpl(_defaultIndent){
  trait DotWriterTest extends DotWriter{
    def noEdgesChaining(edges: Seq[Edge]): Seq[AnyEdge] = edges.toList

    def printNoChaining[R](r: => R): R = chain.doWith(false)(r)
    protected val chain = new ScopedState[Boolean](true)

    override def chainEdges(edges: Seq[Edge]) =
      if(chain.get) super.chainEdges(edges) else noEdgesChaining(edges)
  }

  override val write = new DotWriter with DotWriterTest{
    def defaultIndent = _defaultIndent
  }
}

object TestDotSvgGenerationApp extends GenericAutoGraphvizFactory(50, .1, new TestDotDsl(4)) with App{
  import dsl._

  implicit val format = Svg
  implicit val prog = Dot

  writeAndExec("svg-graph-coloring-visualisation.dot", root.value)
  writeAndExec("svg-graph-coloring-visualisation-control.dot", write.printNoChaining(root.value))

}

object GraphColoringVisualisationApp extends App {
  val dir = "tmp/"
  val name = "graph-coloring"
  val progs = Dot :: Fdp :: Sfdp :: Neato :: Nil
  val outFormat = Svg

  val vis = new GenericAutoGraphvizFactory(30, .1, FdpDsl.indent._4)
  import vis._

  file(Path(dir + name)) withOutputStream File.write.utf8(vis.print)

  progs.foreach{
    pr =>
      execGraphviz(dir + name: Path, name + "-" + pr.command)(outFormat, pr)
  }

}