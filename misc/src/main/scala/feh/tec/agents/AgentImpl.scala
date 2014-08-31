package feh.tec.agents

import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.impl._
import feh.tec.agents.impl.agent.AgentCreation
import feh.util.InUnitInterval

trait AgentImpl extends AgentCreation[DefaultNegotiatingLanguage]
  with impl.agent.PriorityBased[DefaultNegotiatingLanguage]
  with DefaultNegotiatingLanguage.Builder
  with NegotiationSupport.Default
  with ProposalEngine.IteratingAllDomains[DefaultNegotiatingLanguage]
{

  type StateOfNegotiation = ProposalIteratorNegotiationState[DefaultNegotiatingLanguage]

  def newNegotiationState(of: Negotiation) = new StateOfNegotiation{ def negotiation = of }

  protected def issuesExtractor = implicitly

  // should change with time
  protected def isFailure(neg: Negotiation, weighted: Map[Option[Boolean], InUnitInterval]) = {
    val rejectLimit = .9
    def unknownLimit = .2 // todo: should change with time

    weighted(Some(false)) >= rejectLimit ||
      weighted(None) <= unknownLimit && weighted(Some(false)) > .5
  }

  object CreateConstraint{
    def notEquals(vr: Var, in: NegotiationId) = Constraint(vr, in, get(in).currentValues(vr) != )
  }
}
