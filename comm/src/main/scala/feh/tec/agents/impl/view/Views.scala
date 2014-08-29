package feh.tec.agents.impl.view

import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.{ConstraintsView, Var}

/**
  */
class Constraints(val constraints: Set[Constraint[Var]]) extends ConstraintsView{
  protected val constraintsSearch = constraints.groupBy[Var](_.over)

  def satisfies[V <: Var](issue: V, value: V#Tpe) =
    (true /: constraintsSearch.getOrElse(issue, Set())){
      case (acc, Constraint(_, test)) => acc && test(value)
    }
}
