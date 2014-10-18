package feh.tec.agents.light.impl

import feh.tec.agents.light._
import feh.util._

trait DomainIterating[Lang <: Language.ProposalBased] extends ProposalEngine.Iterating[Lang]{
  def domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]]
}

