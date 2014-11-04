package feh.tec.agents.light.impl

import feh.tec.agents.light._
import feh.util._

trait DomainIterating[Lang <: Language.ProposalBased] extends ProposalEngine.Iterating[Lang]{
  def varsByNeg: Map[NegotiationId, Set[Var]]
  def domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]]
}

