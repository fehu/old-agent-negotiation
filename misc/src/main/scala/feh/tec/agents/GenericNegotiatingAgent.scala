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
  with ProposalEngine.IteratingAllDomainsLearningFromMistakes[DefaultNegotiatingLanguage]
  with AgentReportingMessagesAndState[DefaultNegotiatingLanguage]
  with ActorLogging
//  with ProposalEngine.SharingKnowledge[DefaultNegotiatingLanguage]
{
  type StateOfNegotiation = ProposalIteratorNegotiationState[DefaultNegotiatingLanguage] with ProposalViewState

  def newNegotiationState(of: Negotiation): StateOfNegotiation =
    new ProposalIteratorNegotiationState[DefaultNegotiatingLanguage] with ProposalViewState{ def negotiation = of }

  // should change with time
  protected def isFailure(neg: Negotiation, weighted: Map[Option[Boolean], InUnitInterval]) = weighted.get(Some(false)).nonEmpty

  log.info(s"I'm created! $id" )

  override protected def extractStateReportExtra(negId: NegotiationId) = getOpt(negId).flatMap(_.state.currentProposal)
  override protected def extractMessageReportExtra(msg: DefaultNegotiatingLanguage#Msg) =
    get(msg.negotiation).state.lastWeightedProposal map WeightReport

  protected def priorityConflictMsg(causedBy: Message) = {
    implicit def priority = get(causedBy.negotiation).priority
    Message.PriorityConflict(causedBy.negotiation)
  }
  protected def isPriorityConflict(msg: DefaultNegotiatingLanguage#Msg) = msg.isInstanceOf[Message.PriorityConflict]

  def knowledgeShare = arg.knowledgeShare

  def resetIterator(negId: NegotiationId) = {
    val neg = get(negId)
    val vals = neg.currentValues.toMap
    val it = newIterator(negId)
    neg.state.currentIterator = Some(it)
  }
}