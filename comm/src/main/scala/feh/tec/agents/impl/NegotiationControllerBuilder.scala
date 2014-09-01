package feh.tec.agents.impl

import feh.tec.agents
import feh.tec.agents.{Var, NegotiationController}

trait NegotiationControllerBuilder[Control <: NegotiationController]
  extends agents.NegotiationControllerBuilder[NegotiationSpecification, Control]
{
  import NegotiationSpecification._

  protected def buildVars(defs: Seq[VarDef]): Map[String, Var]
}
