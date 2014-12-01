package feh.tec.agents.lite.impl.spec

import feh.tec.agents.lite.Message.IssueChange
import feh.tec.agents.lite._
import feh.tec.agents.lite

trait ChangingIssuesSpec[Ag <: ChangingIssues[Lang], Lang <: Language.ProposalBased with Language.HasPriority with Language.NegotiatesIssues]
  extends lite.spec.ChangingIssuesSpec[Ag, Lang]
{
  lazy val requestIssueAggregation = new DefBADS[(NegotiationId, Seq[Var]) => Unit](
    ag =>
    {
      case (negId, vars) =>
        ag.sendToAll(Message.IssuesRequest(
          negId,
          IssueChange.Add(vars: _*),
          ag.get(negId).currentPriority(),
          ag.get(negId).currentValues()
        )(ag.ref))
    }
  )

  lazy val onIssueRequest = new DefBADS[Lang#IssueRequest => Unit](_ => ???)
  lazy val onIssueResponse = new DefBADS[Lang#IssueResponse => Unit](_ => ???)
}

