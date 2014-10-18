package feh.tec.agents.light.impl.spec

import feh.tec.agents.light.ProposalEngine.DomainIterator
import feh.tec.agents.light._
import feh.tec.agents.light.impl.{PriorityAndProposalBasedAgent, DomainIterating}
import feh.tec.agents.light.spec.AgentSpecification
import feh.util._

object IteratingSpec{
  type Agent[Lang <: Language.ProposalBased with Language.HasPriority] = PriorityAndProposalBasedAgent[Lang] with DomainIterating[Lang] with AgentHelpers[Lang]{
    type Negotiation <: Negotiation.HasProposal[Lang] with Negotiation.HasIterator
  }

  trait AllVars[Ag <: IteratingSpec.Agent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]
    extends AgentSpecification.Iterating[Ag, Lang]
  {
    self: AgentSpecification.PriorityAndProposalBased[Ag, Lang] =>

    lazy val newIterator = new DefBADS[NegotiationId => DomainIterator](
      owner => {
        negId =>
          val neg = owner.get(negId)
          val dItByVar = neg.currentValues().keys.toSeq.zipMap(owner.domainIterators)
          val it = DomainIteratorBuilder overSeq dItByVar.map(_._2)
          val vars = dItByVar.map(_._1)
          val i2i: Seq[Any] => Map[Var, Any] = seq => {
            assert(vars.length == seq.length)
            vars.zip(seq).toMap
          }
          val domains = dItByVar.map(_._1.domain)
          it.apply(domains).map(i2i)

      })

    lazy val nextValues = new DefBADS[NegotiationId => Option[Map[Var, Any]]](
      owner =>
        negId => owner.get(negId).currentIterator.raw.collectFirst({
          case it if it.hasNext => it.next()
        })
    )
  }
}