package feh.tec.agents.lite.impl.agent

import feh.tec.agents.lite.{PartialSolution, NegotiationId, FailedPartialSolutionsChecks, Language}
import scala.collection.mutable

trait PartialSolutionsChecks[Lang <: Language.ProposalBased with Language.HasPriority with Language.NegotiatesIssues]
  extends FailedPartialSolutionsChecks[Lang]
{
  /** Used only by top-priority */
  private lazy val _failedPartialSolutions = mutable.HashMap.empty[NegotiationId, mutable.ListBuffer[PartialSolution]]
    def failedPartialSolutions = _failedPartialSolutions.mapValues(_.toList).toMap

  /** yes / no */
  def repeatingAFailure(ps: PartialSolution) = _failedPartialSolutions.get(ps.negotiation).exists(_.contains(ps))

  def guardFailedPartialSolution(failed: PartialSolution) = _failedPartialSolutions
    .getOrElseUpdate(failed.negotiation, mutable.ListBuffer.empty[PartialSolution]) += failed

  def failedPartialSolutions(neg: NegotiationId): Seq[PartialSolution] = _failedPartialSolutions.get(neg).map(_.toList).getOrElse(Nil)
}
