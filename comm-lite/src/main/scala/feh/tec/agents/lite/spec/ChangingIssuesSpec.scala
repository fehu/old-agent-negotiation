package feh.tec.agents.lite.spec

import feh.tec.agents.lite.{ChangingIssues, Language, NegotiationId, Var}

trait ChangingIssuesSpec[Ag <: ChangingIssues[Lang], Lang <: Language.HasPriority with Language.NegotiatesIssues]
  extends AgentSpecificationExt[Ag]
{
  def requestIssueAggregation: DefBADS[(NegotiationId, Seq[Var]) => Unit]
  def onIssueRequest: DefBADS[Lang#IssueRequest => Unit]
  def onIssueResponse: DefBADS[Lang#IssueResponse => Unit]
}
