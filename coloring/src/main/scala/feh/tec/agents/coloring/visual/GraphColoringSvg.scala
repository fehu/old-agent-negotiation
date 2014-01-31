package feh.tec.agents.coloring.visual

import feh.util._
import feh.dsl.graphviz._
import feh.tec.agents.comm.coloring.{ColoringGraph, GraphGeneratorImpl}
import org.w3c.dom.svg.SVGDocument
import feh.dsl.graphviz.OutFormat.Svg
import org.apache.batik.dom.svg.SAXSVGDocumentFactory
import org.apache.batik.util.XMLResourceDescriptor
import java.io.StringReader
import java.util.UUID

case class GraphvizGraphGen(nNodes: Int, edgeProb: InUnitInterval) {
  lazy val generator = new GraphGeneratorImpl
  lazy val autogenGr = generator.generate("coloring", nNodes, _.nextDouble() < edgeProb.d)
}

trait GraphvizGraphFactory extends GraphvizExec{
  def gr: ColoringGraph  

  class NameGenerator(forbidden: Set[String]){
    protected var next = "a"

    private def incr(str: String) = {
      def inner(str: List[Char]): List[Char] = str match{
        case 'Z' :: tail => 'Z' :: inner(tail)
        case 'z' :: tail => 'A' :: tail
        case az :: tail if 'a' <= az && az < 'z' || 'A' <= az && az < 'Z' => (az.toInt + 1).toChar :: tail
      }

      if(str.distinct == "Z") "a" * (str.length + 1)
      else inner(str.toList.reverse).reverse.mkString
    }

    def nextName: String = {
      val r = next
      next = incr(next)
      r
    }
  }

  val nameGen = new NameGenerator(Set())

  def writeToFile(path: Path) = file(path) withOutputStream File.write.utf8(print)

  def print: String
  def naming: Map[UUID, String]
}

class GenericGraphvizFactory[Dsl <: GraphvizDsl](val dsl: Dsl, val gr: ColoringGraph) extends GraphvizGraphFactory{
  import dsl._
  import Attributes._

  val nodes = gr.nodes.zipMap(_ => nameGen.nextName).map{
    case (node, name) => node.id -> dsl.Node(name, Set(
      Label(node.name.getOrElse(name)),
      Id(name),
      Tooltip(node.id.toString),
      Fill, Color(java.awt.Color.white)
    ))
  }.toMap

  val edges = gr.edges.map{
    edge => Edge(nodes(edge._1), nodes(edge._2))
  }

  val root = Root(gr.name, nodes.values.toSeq ++ edges)

  def print = root.value
  def naming = nodes.mapValues(_.name)
}

class GenericAutoGraphvizFactory[Dsl <: GraphvizDsl](val nNodes: Int,
                                                     val edgeProb: InUnitInterval,
                                                     dsl: Dsl)
  extends GenericGraphvizFactory(dsl, new GraphvizGraphGen(nNodes, edgeProb).autogenGr)

trait SvgLoader extends GraphvizGraphFactory{
  def load(file: Path)(implicit prog: Prog): SVGDocument = {
    implicit def out = Svg
    val svg = readGraphviz(file)
    FileUtils.file("tst.svg") withOutputStream File.write.utf8(svg)

    new SAXSVGDocumentFactory(XMLResourceDescriptor.getXMLParserClassName)
      .createSVGDocument("http://www.w3.org/2000/svg", new StringReader(svg))
  }
}