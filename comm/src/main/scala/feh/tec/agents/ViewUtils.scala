package feh.tec.agents

import feh.util.InUnitInterval

trait ViewUtils {
  utils =>

  def regroup[V, NK, NV](byRef: Map[AgentRef, V], foo: V => (NK, NV)): Map[NK, (NV, AgentRef)] = byRef.map{
    case (k, v) =>
      val (nk, nv) = foo(v)
      nk -> (nv, k)
  }

  def weight[I <: Iterable[T], T, K](what: I, select: PartialFunction[T, K]): Map[K, InUnitInterval] = {
    val size = what.size
    val grouped = what.groupBy(select.orElse{ case _ => null.asInstanceOf[K] }) - null.asInstanceOf[K]
    grouped.mapValues(it => InUnitInterval(it.size / size))
  }

  implicit class ExternalDataMapWrapper[V](map: Map[AgentRef, V]){
    def regroup[NK, NV](foo: V => (NK, NV)) = utils.regroup(map, foo)
  }

  implicit class IterableWrapper[I <: Iterable[T], T](it: I){
    def weight[K](select: PartialFunction[T, K]) = utils.weight(it, select)
  }

}

trait ExternalViewSupport extends InfoGathering{
  def externalViews: Seq[ExternalView]
  def filterIncoming: AbstractMessage => Boolean

  def gatherInfo(msg: AbstractMessage) = if(filterIncoming(msg)) externalViews.foreach(_.process)
}