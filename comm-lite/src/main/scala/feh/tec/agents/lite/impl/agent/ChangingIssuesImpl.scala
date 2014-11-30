package feh.tec.agents.lite.impl.agent

import feh.tec.agents.lite.impl.ChangingIssues
import feh.tec.agents.lite.impl.spec.ChangingIssuesSpec
import feh.tec.agents.lite.spec.AgentSpecification
import feh.tec.agents.lite.{Negotiation, Var, NegotiationId, Language}

trait ChangingIssuesImpl[Lang <: Language.ProposalBased with Language.HasPriority with Language.NegotiatesIssues]
  extends PriorityAndProposalBasedAgent[Lang] with ChangingIssues[Lang]
  {

  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang] with Negotiation.ChangingIssues
  type Agent <: PriorityAndProposalBasedAgent[Lang] with ChangingIssues[Lang]
  override val spec: AgentSpecification.PriorityAndProposalBased[Agent, Lang] with ChangingIssuesSpec[Agent, Lang]

  def requestIssueAggregation(neg: NegotiationId, issues: Var*) = spec.requestIssueAggregation.get

  def onIssueRequest(msg: Lang#IssueRequest) = spec.onIssueRequest.get
//      if(hasTopPriority) {
//      issueRequests.filterNot(_.sender == msg.sender) += msg
//      ???
//    }
  def onIssueResponse(msg: Lang#IssueResponse) = spec.onIssueResponse.get
//  if(msg.priority > get(msg.negotiation).currentPriority()){
//
//    }
//  }

}
