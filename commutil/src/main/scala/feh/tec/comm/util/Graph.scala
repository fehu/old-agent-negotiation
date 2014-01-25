package feh.tec.comm.util

import java.util.UUID
import feh.util._
import feh.tec.comm.util.Graph.Node

object Graph{
  case class Node[T](id: UUID, value: T, neighbours: Set[UUID]){
    def hasNeighbours = neighbours.nonEmpty
    def newValue(v: T) = copy(value = v)
  }

}

abstract class Graph[T](val nodes: Set[Node[T]]){
  def apply(id: UUID) = get(id).get
  def get(id: UUID) = nodes.find(_.id == id)

  def neighbouringNodes(n: Node[T]) =
    n.neighbours.map(id => nodes.find(_.id == id)
      .getOrThrow(s"Incorrect connection: $n with id=$id - no node with such id exists"))
}