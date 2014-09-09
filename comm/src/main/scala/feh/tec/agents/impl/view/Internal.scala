package feh.tec.agents.impl.view

import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.impl.Agent
import feh.tec.agents.{NegotiationId, ConstraintsView, Var}
import feh.util._

/**
  */
class Constraints(val constraints: Set[Constraint]) extends ConstraintsView{
  protected val constraintsSearch = constraints.groupBy[Set[Var]](_.over)

  def satisfy(issues: Map[Var, Any], values: Map[Var, Any]) =
    (true /: constraintsSearch.getOrElse(issues.keySet, Set())){
      case (acc, Constraint(_, _, _, depends, test)) =>
        acc && test(issues, depends.zipMap(values).toMap)
    }
}
