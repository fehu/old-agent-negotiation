package feh.tec.agents.light

import impl.agent._

object AgentManualSpecApp {
  val agentSpec = new create.PPI.AllVarsSpec{

      start <:= {
        ag =>
          import ag._
          negotiations.foreach{
            neg =>
              val proposal = neg.currentProposal.getOrElse{
                Message.Proposal(Message.ProposalId.rand, neg.id, neg.currentPriority(), neg.currentValues())
              }
              sendToAll(proposal)
          }
      }
    }

  val aProps =
    create.AgentProps("test", NegotiationRole("test"), Set(), create.PriorityAndProposalBasedIteratingAllVars(agentSpec))
}