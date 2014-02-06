package feh.tec.agents.visualisation.util

import java.util.UUID
import feh.util._
import feh.tec.agents.visualisation.util.Graph.{GraphRef, Node}
import scala.collection.mutable

object Graph{
  private val register = mutable.Map.empty[GraphRef, Graph[_]]
  protected def register(gr: Graph[_]){
    register += gr.ref -> gr
  }

  case class Node[T](graph: GraphRef, id: UUID, value: T, name: Option[String] = None){
    def neighbours = graph.neighbours(this)

    def hasNeighbours = neighbours.nonEmpty
    def newValue(v: T) = copy(value = v)
  }

  case class GraphRef(name: String, id: UUID = UUID.randomUUID()){
    def neighbours[T](node: Node[T]) = register(this).neighbouringNodes(node.id).map(_.asInstanceOf[Node[T]])
  }
}

abstract class Graph[T](val ref: GraphRef, val nodes: Set[Node[T]], val edges: Set[(UUID, UUID)]){
  Graph.register(this)

  def name = ref.name

  def apply(id: UUID) = get(id).get
  def get(id: UUID) = nodes.find(_.id == id)

  def neighbouringNodes(of: UUID): Set[Node[T]]
  def neighbouringNodes(n: Node[T]): Set[Node[T]] = neighbouringNodes(n.id)
  def byId(id: UUID) = nodes.find(_.id == id)
  def getById(id: UUID) = byId(id).get
}

trait Undirected[T] extends Graph[T]{
  def neighbouringNodes(of: UUID) = edges.collect{
    case (`of`, other) => other
    case (other, `of`) => other
  }.toSet.map(getById)
}