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

/** Holds implementation specific variables, that need no initial values
 */
trait NegotiationState{
  def negotiation: Negotiation
}

trait NegotiationStateSupport{
  self: NegotiatingAgent =>

  type StateOfNegotiation <: NegotiationState

  def newNegotiationState(of: Negotiation): StateOfNegotiation
  
  val negotiationStates = negotiations.map(ng => ng.id -> newNegotiationState(ng)).toMap

  implicit class NegotiationSyntaxExtender(n: Negotiation){
    def state = negotiationStates(n.id)
  }
}

class DynamicScopeNegotiation(val id: NegotiationId,
                              initPriority: Priority,
                              vars: Set[Var])
  extends Negotiation with DynamicScope
{
  implicit var currentPriority: Priority = initPriority
  val currentValues: mutable.HashMap[Var, Any] = mutable.HashMap(vars.toSeq.map(_ -> null): _*)
  var currentValuesAcceptance = false

  protected val _scope = mutable.HashSet.empty[AgentRef]

  def scope = _scope.toSet

  def updateScope(msg: ScopeUpdate) = msg match {
    case ScopeUpdate.NewScope(scope, neg) if neg == id =>
      _scope.clear()
      _scope ++= scope
    case ScopeUpdate.NewAgents(refs, neg) if neg == id => _scope ++= refs
    case ScopeUpdate.RmAgents(refs, neg)  if neg == id => _scope --= refs
  }

  def resetPriority() = currentPriority = initPriority
}

abstract class NegotiationException(msg: String) extends Exception(msg)

case class UnknownNegotiation(id: NegotiationId)(implicit val self: AgentRef) extends NegotiationException(
  s"No negotiation with $id found in agent $self"
)

case class ScopeUpdateException(id: NegotiationId, reason: String)(implicit val self: AgentRef) extends NegotiationException(
  s"Failed to update the scope of $id in $self: $reason"
)