package feh.tec.agents.lite.impl

import feh.tec.agents.lite._
import feh.util._

trait DomainIterating[Lang <: Language.ProposalBased] extends ProposalEngine.Iterating[Lang]{
  def varsByNeg: Map[NegotiationId, Set[Var]]
  def domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]]
}

