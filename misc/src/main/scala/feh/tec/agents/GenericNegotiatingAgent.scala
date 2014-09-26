package feh.tec.agents

import akka.actor.ActorLogging
import feh.tec.agents.impl.Agent.AgentReportingMessagesAndState
import feh.tec.agents.impl.AgentReports.WeightReport
import feh.tec.agents.impl._
import feh.tec.agents.impl.agent.GenericIteratingAgentCreation
import feh.util.InUnitInterval

class GenericNegotiatingAgent(arg: GenericIteratingAgentCreation.Args)
  extends GenericIteratingAgentCreation[DefaultNegotiatingLanguage](arg)
  with DefaultNegotiatingLanguage.Builder
  with NegotiationSupport.Default
  with ProposalEngine.IteratingAllDomains[DefaultNegotiatingLanguage]
  with AgentReportingMessagesAndState[DefaultNegotiatingLanguage]
  with ActorLogging
{
  type StateOfNegotiation = ProposalIteratorNegotiationState[DefaultNegotiatingLanguage] with ProposalViewState

  def newNegotiationState(of: Negotiation): StateOfNegotiation =
    new ProposalIteratorNegotiationState[DefaultNegotiatingLanguage] with ProposalViewState{ def negotiation = of }

  // should change with time
  protected def isFailure(neg: Negotiation, weighted: Map[Option[Boolean], InUnitInterval]) = {
//    val rejectLimit = .9
//    def unknownLimit = .2 // todo: should change with time

    weighted.get(Some(false)).nonEmpty
//      .exists(_ >= rejectLimit)  ||
//      weighted.get(None).exists(_ <= unknownLimit) && weighted.get(Some(false)).exists(_ > .5)
  }

  log.info(s"I'm created! $id" )

  override protected def extractStateReportExtra(negId: NegotiationId) = getOpt(negId).flatMap(_.state.currentProposal)
  override protected def extractMessageReportExtra(msg: DefaultNegotiatingLanguage#Msg) =
    get(msg.negotiation).state.lastWeightedProposal map WeightReport

  protected def priorityConflictMsg(causedBy: Message) = {
    implicit def priority = get(causedBy.negotiation).priority
    Message.PriorityConflict(causedBy.negotiation)
  }
  protected def isPriorityConflict(msg: DefaultNegotiatingLanguage#Msg) = msg.isInstanceOf[Message.PriorityConflict]
}