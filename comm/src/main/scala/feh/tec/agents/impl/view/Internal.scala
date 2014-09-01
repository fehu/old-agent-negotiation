package feh.tec.agents.impl.view

import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.impl.Agent
import feh.tec.agents.{NegotiationId, ConstraintsView, Var}

/**
  */
class Constraints(val constraints: Set[Constraint[Var]]) extends ConstraintsView{
  protected val constraintsSearch = constraints.groupBy[Var](_.over)

  def satisfies(issue: Var, value: Any) =
    (true /: constraintsSearch.getOrElse(issue, Set())){
      case (acc, Constraint(_, _, test)) => acc && issue.cast(value).exists(test)
    }
}

trait CreateConstraintsHelper{
  self: Agent[_] =>

  object CreateConstraint{
    def equals(vr: Var, in: NegotiationId) = Constraint(vr, in, get(in).currentValues(vr) == )
    def notEquals(vr: Var, in: NegotiationId) = Constraint(vr, in, get(in).currentValues(vr) != )
  }

}