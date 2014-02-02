package feh.tec.agents.visualisation.util

import java.util.UUID
import feh.util._
import feh.tec.agents.visualisation.util.Graph.Node
import scala.collection.mutable

object Graph{
  case class Node[T](id: UUID, value: T, neighbours: Set[UUID], name: Option[String] = None){
    def hasNeighbours = neighbours.nonEmpty
    def newValue(v: T) = copy(value = v)
  }

}

abstract class Graph[T](val name: String, val nodes: Set[Node[T]]){
  def apply(id: UUID) = get(id).get
  def get(id: UUID) = nodes.find(_.id == id)

  def neighbouringNodes(of: UUID): Option[Set[Node[T]]] = byId(of).map(neighbouringNodes)
  def neighbouringNodes(n: Node[T]): Set[Node[T]] =
    n.neighbours.map(id => nodes.find(_.id == id)
      .getOrThrow(s"Incorrect connection: $n with id=$id - no node with such id exists"))

  def edges: Set[(UUID, UUID)] = (mutable.HashSet.empty[(UUID, UUID)] /: nodes){
    (acc, n) =>
      acc ++= n.neighbours.withFilter(id => !acc.exists({
        case (u1, u2) => u1 == id && u2 == n.id || u2 == id && u1 == n.id
      })).map(n.id ->)
  }.toSet
  
  def byId(id: UUID) = nodes.find(_.id == id)
  def getById(id: UUID) = byId(id).get
}