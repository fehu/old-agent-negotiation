package feh.tec.agents.lite.impl

import akka.actor.Actor
import feh.tec.agents.lite._

trait ChangingIssues[Lang <: /*Language.ProposalBased with*/ Language.HasPriority with Language.NegotiatesIssues]
  extends NegotiatingAgent[Lang] with AgentHelpers[Lang]
{
  type Negotiation <: Negotiation.HasPriority with Negotiation.ChangingIssues

  abstract override def process = ({
    case msg: Lang#IssueRequest   =>
    case msg: Lang#IssueResponse  =>
  }: Actor.Receive) orElse super.process

  def requestIssueAggregation(neg: NegotiationId, issues: Var*)

  def onIssueRequest(msg: Lang#IssueRequest)
  def onIssueResponse(msg: Lang#IssueResponse)
}
