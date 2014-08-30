package feh.tec.agents

import feh.util.InUnitInterval

trait ViewUtils {
  utils =>

  private def doTheGrouping[NK, NV](it: Iterable[(AgentRef, (NK, NV))]) =
    it.groupBy(_._2._1).mapValues{
      _.map{ case (ag, (k, v)) => ag -> v}.toMap
    }

  def regroup[V, NK, NV](byRef: Map[AgentRef, V], foo: V => (NK, NV)): Map[NK, Map[AgentRef, NV]] =
    doTheGrouping(byRef.mapValues(foo))

  def regroupSeq[V, NK, NV](byRef: Map[AgentRef, V], foo: V => Seq[(NK, NV)]): Map[NK, Map[AgentRef, NV]] =
    doTheGrouping( byRef.toSeq.flatMap{ case (k, v) => foo(v) map (k -> _) } )

  def weight[I <: Iterable[T], T, K](what: I, select: PartialFunction[T, K]): Map[K, InUnitInterval] = {
    val size = what.size
    val grouped = what.groupBy(select.orElse{ case _ => null.asInstanceOf[K] }) - null.asInstanceOf[K]
    grouped.mapValues(it => InUnitInterval(it.size / size))
  }

  implicit class ExternalDataMapWrapper[V](map: Map[AgentRef, V]){
    def regroup[NK, NV](foo: V => (NK, NV)) = utils.regroup(map, foo)
    def regroupSeq[NK, NV](foo: V => Seq[(NK, NV)]) = utils.regroupSeq(map, foo)
  }

  implicit class IterableWrapper[T](it: Iterable[T]){
    def weight[K](select: PartialFunction[T, K]) = utils.weight(it, select)
  }

}

trait ExternalViewSupport extends InfoGathering{
  def externalViews: Seq[ExternalView]
  def filterIncoming: AbstractMessage => Boolean

  def gatherInfo(msg: AbstractMessage) = if(filterIncoming(msg)) externalViews.foreach(_.process)
}