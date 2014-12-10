package feh.tec.agents.lite.impl.agent

import feh.tec.agents.lite._

trait NegotiationNotifications extends NegotiationSupport{
  self: AgentHelpers[_] with NegotiatingAgent[_] with SpeakingAgent[_] with SystemSupport =>

  protected def controllerRef: AgentRef

  def finished(negotiation: NegotiationId, result: Solution) = controllerRef.ref !
    SystemMessage.NegotiationFinished(negotiation, result.values.values.toSeq)

  def failed(negotiation: NegotiationId, reason: Any) = controllerRef.ref !
    SystemMessage.NegotiationFailed(negotiation, reason)
}
