package feh.tec.agents.visualisation

import att.grappa
import feh.tec.comm.util.Graph

trait GraphToGrappa[T] {
  def grappaGraph(gr: Graph[T]): grappa.Graph
}
