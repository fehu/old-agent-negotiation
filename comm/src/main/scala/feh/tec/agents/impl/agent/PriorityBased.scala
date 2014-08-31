package feh.tec.agents.impl.agent

import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents._
import feh.tec.agents.impl._
import feh.tec.agents.impl.view.{Constraints, ConstraintsSatisfactionWithPriority}
import feh.util._


trait PriorityBased[Lang <: ProposalLanguage] extends AgentCreation[Lang]
  with PriorityBasedAgent[Lang]
  with impl.Agent.ProposalRegistering[Lang]
  with ProposalEngine[Lang] 
  with PriorityBasedAgentViews
  with NegotiationStateSupport
  with ViewUtils
{
  lazy val constraintsSatisfactions = ConstraintsSatisfactionWithPriority(lang)
  lazy val proposalSatisfaction = new Constraints(constraints)

  def constraints: Set[Constraint[Var]]
  protected def issuesExtractor: IssuesExtractor[Lang]

  protected def isFailure(neg: Negotiation, weighted: Map[Option[Boolean], InUnitInterval]): Boolean

  def accept_?(prop: Lang#Proposal) = issuesExtractor.extract(prop) forall (proposalSatisfaction.satisfies _).tupled
  
  def checkConstraints(negId: NegotiationId) = get(negId) |> {
    neg =>
      neg.state.currentProposal map {
        currentProposal =>
          val viewsByMsgId = constraintsSatisfactions.data.regroupSeq{
            case (mp, pr) => mp.toSeq.map{
              case (msgId, opt) => msgId -> (opt, pr)
            }
          }
          val weighted = viewsByMsgId(currentProposal.id).weight{
            case (ag, (opt, pr)) if neg.scope.contains(ag) && pr._2 > neg.currentPriority => opt
          }

          if(isFailure(neg, weighted))
            setNextProposal(neg)
              .map( _ => spamProposal _ )
              .getOrElse( noMoreProposals _ )
              .apply(neg)
      }
  }

  override def startLife() = negotiations foreach {
    neg =>
      resetProposal(neg)
      spamProposal(neg)
  }

  protected def spamProposal(neg: ANegotiation) = sendToAll(neg, neg.state.currentProposal.asInstanceOf[Lang#Msg])

  protected def maxPriority(negId: NegotiationId) = constraintsSatisfactions.merge._2
    .data.filter(_._2._1 == negId).map(_._2._2).maxBy(_.get)

  protected def noMoreProposals(neg: ANegotiation) = {
    neg.currentPriority = maxPriority(neg.id).raise()
    resetProposal(neg)
    spamProposal(neg)
  }
}
