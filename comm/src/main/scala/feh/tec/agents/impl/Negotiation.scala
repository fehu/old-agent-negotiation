package feh.tec.agents.impl

import feh.tec.agents.SystemMessage.ScopeUpdate
import feh.tec.agents._

import scala.collection.mutable

class StaticScopeNegotiation(val id: NegotiationId,
                             val scope: Set[AgentRef],
                             initPriority: Priority,
                             initVals: Map[Var, Any])
  extends Negotiation with StaticScope
{
  implicit var currentPriority: Priority = initPriority
  val vals: mutable.HashMap[Var, Any] = mutable.HashMap(initVals.toSeq: _*)
}

class DynamicScopeNegotiation(val id: NegotiationId,
                              initScope: Set[AgentRef],
                              initPriority: Priority,
                              initVals: Map[Var, Any])
  extends Negotiation with DynamicScope
{
  implicit var currentPriority: Priority = initPriority
  val vals: mutable.HashMap[Var, Any] = mutable.HashMap(initVals.toSeq: _*)

  protected val _scope = mutable.HashSet.empty[AgentRef]

  protected def scopeProvider = () => scope.toSet

  def updateScope(msg: ScopeUpdate) = msg match {
    case ScopeUpdate.NewScope(scope, neg, _) if neg == id =>
      _scope.clear()
      _scope ++= scope
    case ScopeUpdate.NewAgents(refs, neg, _) if neg == id => _scope ++= refs
    case ScopeUpdate.RmAgents(refs, neg, _)  if neg == id => _scope --= refs
  }
}

trait DynNegotiationSupport[Lang <: Language] extends NegotiatingAgent[Lang]{

  abstract override def process(sys: SystemMessage) = sys match {
    case upd: ScopeUpdate =>
      val neg = negotiations
        .find(_.id == upd.negotiation)
        .getOrElse( throw UnknownNegotiation(upd.negotiation) )
      neg match {
        case dyn: DynamicScopeNegotiation => dyn.updateScope(upd)
        case _ => throw ScopeUpdateException(neg.id, "isn't DynamicScopeNegotiation")
      }
    case other => super.process(other)
  }
}

abstract class NegotiationException(msg: String) extends Exception(msg)

case class UnknownNegotiation(id: NegotiationId)(implicit val self: AgentRef) extends NegotiationException(
  s"No negotiation with $id found in agent $self"
)

case class ScopeUpdateException(id: NegotiationId, reason: String)(implicit val self: AgentRef) extends NegotiationException(
  s"Failed to update the scope of $id in $self: $reason"
)