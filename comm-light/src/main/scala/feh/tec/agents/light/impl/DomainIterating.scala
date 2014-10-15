package feh.tec.agents.light.impl

import feh.tec.agents.light._
import feh.util._

trait DomainIterating[Lang <: Language.ProposalBased with Language.HasPriority] extends ProposalEngine.Iterating[Lang]{
  def domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]]
}

object DomainIterating{

  trait AllVars[Lang <: Language.ProposalBased with Language.HasPriority] extends DomainIterating[Lang]{
    self: AgentHelpers[Lang] =>

    def newIterator(negId: NegotiationId): DomainIterator = {
      val neg = get(negId)
      val dItByVar = neg.currentValues().keys.toSeq.zipMap(domainIterators)
      val it = DomainIteratorBuilder overSeq dItByVar.map(_._2)
      val vars = dItByVar.map(_._1)
      val i2i: Seq[Any] => Map[Var, Any] = seq => {
        assert(vars.length == seq.length)
        vars.zip(seq).toMap
      }
      val domains = dItByVar.map(_._1.domain)
      it.apply(domains).map(i2i)
    }
  }

  trait Default[Lang <: Language.ProposalBased with Language.HasPriority] { // spec

  }
}

