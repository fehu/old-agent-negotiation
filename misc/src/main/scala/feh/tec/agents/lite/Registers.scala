package feh.tec.agents.lite

import feh.tec.agents.lite.Message.ProposalId
import feh.tec.agents.lite.impl.PriorityAndProposalBasedAgent
import feh.tec.agents.lite.impl.agent.create.SpecExt
import feh.tec.agents.lite.impl.spec.PriorityAndProposalBasedAgentSpec
import feh.util._
import scala.collection.mutable

trait PrioritiesRegister[Agent <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]{
  self: PriorityAndProposalBasedAgentSpec[Agent, Lang] with SpecExt[Agent] =>

  val priorities = mutable.HashMap.empty[NegotiationId, mutable.HashMap[AgentRef, Option[Priority]]]

  def allPrioritiesKnown(neg: NegotiationId) = priorities(neg).forall(_._2.isDefined)
  def maxPriority(neg: NegotiationId)(implicit ag: Agent) =
    (priorities(neg) + (ag.ref -> ag.get(neg).currentPriority.raw))
      .mapValues(_.get).maxBy(_._2.get)

  beforeEachMessage andThen {
    ag => overridden => msg =>
      overridden(ag)(msg)
      priorities
        .getOrElseUpdate(
          msg.negotiation, mutable.HashMap(ag.get(msg.negotiation).scope().toSeq.zipMap(_ => Option.empty[Priority]): _*)
        ) += msg.sender -> Some(msg.priority)
  }
}

trait ProposalAcceptanceRegister[Agent <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]{
  self: PriorityAndProposalBasedAgentSpec[Agent, Lang] with SpecExt[Agent] =>

  private val _proposalAcceptance = mutable.HashMap.empty[NegotiationId, (ProposalId, mutable.HashMap[AgentRef, Option[(Message.ProposalResponse, Boolean)]])]
  def proposalAcceptance = _proposalAcceptance.mapValues{case (id, m) => id -> m.toMap}.toMap

  def setProposalAcceptance(msg: Message.ProposalResponse, v: Boolean)(implicit ag: Agent) = {
    val (pid, map) = _proposalAcceptance
      .getOrElseUpdate(msg.negotiation, msg.respondingTo -> mutable.HashMap(ag.get(msg.negotiation).scope().zipMap(_ => Option.empty[(Message.ProposalResponse, Boolean)]).toSeq: _*))
    assert(pid == msg.respondingTo)
    map(msg.sender) = Option(msg -> v)
  }

  def clearProposalAcceptance(neg: NegotiationId)(implicit ag: Agent) = _proposalAcceptance -= neg

  def allAccepted(neg: NegotiationId, pid: ProposalId) =
    _proposalAcceptance.get(neg).exists{
      case (id, map) => id == pid  && map.nonEmpty && map.forall(_._2.exists(_._2))
    }

  updateCurrentProposal andThen {
    ag =>
      overridden =>
        negId =>
          clearProposalAcceptance(negId)(ag)
          overridden(ag)(negId)
  }

}
