package feh.tec.agents.impl

import feh.tec.agents.SystemMessage.ScopeUpdate
import feh.tec.agents._
import feh.tec.agents.impl.Negotiation.DynamicScope

import scala.collection.mutable

object Negotiation{

  trait DynamicScope{
    self: Negotiation =>

    def updateScope(msg: ScopeUpdate)
  }

}

/*
class StaticScopeNegotiation(val id: NegotiationId,
                             val scope: Set[AgentRef],
                             initPriority: Priority,
                             initVals: Map[Var, Any])
  extends Negotiation with StaticScope
{
  implicit var currentPriority: Priority = initPriority
  val vals: mutable.HashMap[Var, Any] = mutable.HashMap(initVals.toSeq: _*)
}
*/

class DynamicScopeNegotiation(val id: NegotiationId,
                              initPriority: Priority,
                              initVals: Map[Var, Any])
  extends Negotiation with DynamicScope
{
  implicit var currentPriority: Priority = initPriority
  val vals: mutable.HashMap[Var, Any] = mutable.HashMap(initVals.toSeq: _*)

  protected val _scope = mutable.HashSet.empty[AgentRef]

  def scope = scope.toSet

  def updateScope(msg: ScopeUpdate) = msg match {
    case ScopeUpdate.NewScope(scope, neg, _) if neg == id =>
      _scope.clear()
      _scope ++= scope
    case ScopeUpdate.NewAgents(refs, neg, _) if neg == id => _scope ++= refs
    case ScopeUpdate.RmAgents(refs, neg, _)  if neg == id => _scope --= refs
  }
}

/*
trait DynNegotiationSupport[Lang <: Language] extends NegotiatingAgent{

  abstract override def processSys = {
    case upd: ScopeUpdate =>
      val neg = negotiations
        .find(_.id == upd.negotiation)
        .getOrElse( throw UnknownNegotiation(upd.negotiation) )
      neg match {
        case dyn: DynamicScopeNegotiation => dyn.updateScope(upd)
        case _ => throw ScopeUpdateException(neg.id, "isn't DynamicScopeNegotiation")
      }
    case other => super.processSys(other)
  }
}
*/

abstract class NegotiationException(msg: String) extends Exception(msg)

case class UnknownNegotiation(id: NegotiationId)(implicit val self: AgentRef) extends NegotiationException(
  s"No negotiation with $id found in agent $self"
)

case class ScopeUpdateException(id: NegotiationId, reason: String)(implicit val self: AgentRef) extends NegotiationException(
  s"Failed to update the scope of $id in $self: $reason"
)