package feh.tec.agents

import scala.collection.mutable

/** Merges views' data and handles forwarding
  *  if no data is present for an agent in a merged view, `null` is passed to `mergeData` in it's place
  */
sealed trait ViewMerge extends ExternalView{
  type Merge <: Product

  def merge: Merge

  def filterIncoming: AbstractMessage => Boolean
  def filterForward: ExternalView => Boolean

  protected val _refs = mutable.HashSet.empty[AgentRef]
  protected val _data = mutable.HashMap.empty[AgentRef, Data]

  def data = _data.toMap
  def mergeSet: Set[ExternalView] = merge.productIterator.toSet.asInstanceOf[Set[ExternalView]]
  def aspect = s"Merge$merge"

  def process = {
    case msg if filterIncoming(msg) =>
      _refs += msg.sender
      mergeSet.withFilter(filterForward).foreach(_.process lift msg)
      updateData()
//      sys.error("_data = " + data)
  }

  def reset() = mergeSet.foreach(_.reset())

  protected def getData[V <: ExternalView](select: Merge => V)(implicit ref: AgentRef): V#Data =
    select(merge).data.getOrElse(ref, null.asInstanceOf[V#Data])

  protected def updateData()
}

object ViewMerge{
  class _2[V1 <: ExternalView, V2 <: ExternalView, D](
          v1: V1,
          v2: V2)
         (_mergeData: => PartialFunction[(V1#Data, V2#Data), D],
          val filterForward: ExternalView => Boolean = _ => true,
          val filterIncoming: AbstractMessage => Boolean = ! _.isInstanceOf[SystemMessage]
  ) extends ViewMerge
  {
    type Data = D
    type Merge = (V1, V2)

    val merge = (v1, v2)
    lazy val mergeData = _mergeData

    protected def updateData() = _data ++=
      _refs.flatMap( implicit ref => mergeData.lift(getData(_._1) -> getData(_._2)).map(ref -> _) )
  }

/* todo refactor
  class _3[D, T1 <: ExternalView, T2 <: ExternalView, T3 <: ExternalView](
          val merge: (T1, T2, T3),
          val mergeData: PartialFunction[(T1#Data, T2#Data, T3#Data), D],
          val filterForward: ExternalView => Boolean = _ => true,
          val filterIncoming: AbstractMessage => Boolean = ! _.isInstanceOf[SystemMessage]
  ) extends ViewMerge
  {
    type Data = D
    type Merge = (T1, T2, T3)

    protected def updateData() = _data ++=
      _refs.flatMap( implicit ref => mergeData.lift(getData(_._1), getData(_._2), getData(_._3)).map(ref -> _) )
  }
*/

}