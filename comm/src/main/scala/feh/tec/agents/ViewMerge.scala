package feh.tec.agents

import scala.collection.mutable

/** Merges views' data and handles forwarding
  *  if no data is present for an agent in a merged view, `null` is passed to `mergeData` in it's place
  */
sealed trait ViewMerge extends ExternalView{
  type Merge <: Product

  def merge: Merge

  protected val _refs = mutable.HashSet.empty[AgentRef]
  protected val _data = mutable.HashMap.empty[AgentRef, Data]

  def data = _data.toMap
  def mergeSet: Set[ExternalView] = merge.productIterator.toSet.asInstanceOf[Set[ExternalView]]
  def aspect = s"Merge$merge"

  def process(msg: AbstractMessage) = if (filterIncoming(msg)) {
    _refs += msg.sender
    mergeSet.foreach(_ process msg)
    updateData()
  }

  protected def getData[V <: ExternalView](select: Merge => V)(implicit ref: AgentRef): V#Data =
    select(merge).data.getOrElse(ref, null.asInstanceOf[V#Data])

  def filterIncoming: AbstractMessage => Boolean
  protected def updateData()
}

object ViewMerge{
  class _2[D, T1 <: ExternalView, T2 <: ExternalView](
                                                       val merge: (T1, T2),
                                                       val mergeData: PartialFunction[(T1#Data, T2#Data), D],
                                                       val filterIncoming: AbstractMessage => Boolean = ! _.isInstanceOf[SystemMessage]
                                                       ) extends ViewMerge
  {
    type Data = D
    type Merge = (T1, T2)

    protected def updateData() = _data ++=
      _refs.flatMap( implicit ref => mergeData.lift(getData(_._1) -> getData(_._2)).map(ref -> _) )
  }

  class _3[D, T1 <: ExternalView, T2 <: ExternalView, T3 <: ExternalView](
                                                                           val merge: (T1, T2, T3),
                                                                           val mergeData: PartialFunction[(T1#Data, T2#Data, T3#Data), D],
                                                                           val filterIncoming: AbstractMessage => Boolean = ! _.isInstanceOf[SystemMessage]
                                                                           ) extends ViewMerge
  {
    type Data = D
    type Merge = (T1, T2, T3)

    protected def updateData() = _data ++=
      _refs.flatMap( implicit ref => mergeData.lift(getData(_._1), getData(_._2), getData(_._3)).map(ref -> _) )
  }

}