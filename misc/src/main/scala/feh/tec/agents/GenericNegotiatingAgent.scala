package feh.tec.agents

import akka.actor.ActorLogging
import feh.tec.agents.impl.Agent.AgentReportingMessagesAndState
import feh.tec.agents.impl._
import feh.util.InUnitInterval

class GenericNegotiatingAgent(arg: GenericIteratingAgentCreation.Args)
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
//    val rejectLimit = .9
//    def unknownLimit = .2 // todo: should change with time

    weighted.get(Some(false)).nonEmpty
//      .exists(_ >= rejectLimit)  ||
//      weighted.get(None).exists(_ <= unknownLimit) && weighted.get(Some(false)).exists(_ > .5)
  }

  log.info(s"I'm created! $id" )

  override protected def extractReportExtra(negId: NegotiationId) = getOpt(negId).flatMap(_.state.currentProposal)

}