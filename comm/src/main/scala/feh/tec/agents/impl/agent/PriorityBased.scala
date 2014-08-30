package feh.tec.agents.impl.agent

import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.Message.Proposal
import feh.tec.agents._
import feh.tec.agents.impl.view.{ConstraintsSatisfactionWithPriority, Constraints}
import feh.tec.agents.impl._
import feh.util._


trait PriorityBased[Lang <: ProposalLanguage] extends AgentCreation[Lang]
  with PriorityBasedAgent[Lang]
  with impl.Agent.ProposalRegistering[Lang]
  with PriorityBasedAgentViews
  with NegotiationStateSupport
  with ViewUtils
{
  type StateOfNegotiation <: PriorityBased.NegotiationState

  lazy val constraintsSatisfactions = ConstraintsSatisfactionWithPriority(lang)
  lazy val proposalSatisfaction = new Constraints(constraints)

  def accept(prop: Lang#Proposal) = issuesExtractor.extract(prop) forall (proposalSatisfaction.satisfies _).tupled

  def constraints: Set[Constraint[Var]]
  protected def issuesExtractor: IssuesExtractor[Lang]

  protected def isFailure(weighted: Map[Option[Boolean], InUnitInterval]): Boolean
  protected def setNextProposal(neg: ANegotiation): Option[Lang#Proposal]
  protected def resetProposal(neg: ANegotiation)

  def checkConstraints(negId: NegotiationId) = get(negId) |> {
    neg =>
      val viewsByMsgId = constraintsSatisfactions.data.regroupSeq{
        case (mp, pr) => mp.toSeq.map{
          case (msgId, opt) => msgId -> (opt, pr)
        }
      }
      val currentProposal = neg.state.currentProposal

      val weighted = viewsByMsgId(currentProposal.id).weight{
        case (ag, (opt, pr)) if neg.scope.contains(ag) && pr > neg.currentPriority => opt
      }

      if(isFailure(weighted))
        setNextProposal(neg)
          .map( _ => spamProposal _ )
          .getOrElse( noMoreProposals _ )
          .apply(neg)
  }

  override def startLife() = negotiations foreach {
    neg =>
      resetProposal(neg)
      spamProposal(neg)
  }

  protected def spamProposal(neg: ANegotiation) = sendToAll(neg, neg.state.currentProposal.asInstanceOf[Lang#Msg])

  protected def maxPriority = constraintsSatisfactions.merge._2.data.maxBy(_._2.get)._2

  protected def noMoreProposals(neg: ANegotiation) = {
    neg.currentPriority = maxPriority.raise()
    resetProposal(neg)
    spamProposal(neg)
  }
}

object PriorityBased{
  trait NegotiationState extends impl.NegotiationState{
    var currentProposal: Proposal = null
  }
}