package feh.tec.agents

import akka.actor.ActorLogging
import feh.tec.agents.impl.Agent.AgentReportingMessagesAndState
import feh.tec.agents.impl._
import feh.tec.agents.impl.agent.AgentCreation
import feh.tec.agents.impl.view.CreateConstraintsHelper
import feh.util.InUnitInterval

trait AgentImpl extends AgentCreation[DefaultNegotiatingLanguage]
  with impl.agent.PriorityBased[DefaultNegotiatingLanguage]
  with DefaultNegotiatingLanguage.Builder
  with NegotiationSupport.Default
  with ProposalEngine.IteratingAllDomains[DefaultNegotiatingLanguage]
  with CreateConstraintsHelper
{

  type StateOfNegotiation = ProposalIteratorNegotiationState[DefaultNegotiatingLanguage]

  def newNegotiationState(of: Negotiation) = new StateOfNegotiation{ def negotiation = of }


  // should change with time
  protected def isFailure(neg: Negotiation, weighted: Map[Option[Boolean], InUnitInterval]) = {
    val rejectLimit = .9
    def unknownLimit = .2 // todo: should change with time

    weighted(Some(false)) >= rejectLimit ||
      weighted(None) <= unknownLimit && weighted(Some(false)) > .5
  }

}

class GenericNegotiatingAgentImpl(arg: GenericIteratingAgentCreation.Args)
  extends impl.GenericIteratingAgentCreation[DefaultNegotiatingLanguage](arg)
  with DefaultNegotiatingLanguage.Builder
  with NegotiationSupport.Default
  with ProposalEngine.IteratingAllDomains[DefaultNegotiatingLanguage]
  with AgentReportingMessagesAndState[DefaultNegotiatingLanguage]
  with ActorLogging
{
  type StateOfNegotiation = ProposalIteratorNegotiationState[DefaultNegotiatingLanguage]

  def newNegotiationState(of: Negotiation): StateOfNegotiation = new StateOfNegotiation{ def negotiation = of }

  // should change with time
  protected def isFailure(neg: Negotiation, weighted: Map[Option[Boolean], InUnitInterval]) = {
    val rejectLimit = .9
    def unknownLimit = .2 // todo: should change with time

    weighted(Some(false)) >= rejectLimit ||
      weighted(None) <= unknownLimit && weighted(Some(false)) > .5
  }

  log.info(s"I'm created! $id" )

  override protected def extractReportExtra(negId: NegotiationId) = getOpt(negId).flatMap(_.state.currentProposal)

}