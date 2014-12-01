package feh.tec.agents.lite

import akka.actor.Actor

trait ChangingIssues[Lang <: Language.HasPriority with Language.NegotiatesIssues]
  extends NegotiatingAgent[Lang] with AgentHelpers[Lang]
{
  type Negotiation <: Negotiation.HasPriority with Negotiation.ChangingIssues

  abstract override def process = ({
    case msg: Lang#IssueRequest   => onIssueRequest(msg)
    case msg: Lang#IssueResponse  => onIssueResponse(msg)
  }: Actor.Receive) orElse super.process

  def requestIssueAggregation(neg: NegotiationId, issues: Var*)

  def onIssueRequest(msg: Lang#IssueRequest)
  def onIssueResponse(msg: Lang#IssueResponse)
}
