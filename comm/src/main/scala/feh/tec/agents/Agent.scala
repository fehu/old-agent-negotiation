package feh.tec.agents

trait AgentHelpers[Lang <: Language]{
  self: NegotiatingAgent with SpeakingAgent[Lang] =>

  def sendToAll(neg: Negotiation, msg: Lang#Msg) = neg.scope.foreach(_ ! msg)

  protected var negotiationsCache = negotiations.map(n => n.id -> n).toMap
  def get(neg: NegotiationId) = negotiationsCache(neg)
  def getOpt(neg: NegotiationId) = negotiationsCache.get(neg)
}


trait PriorityBasedNegotiatingAgent[Lang <: ProposalLanguage]
  extends NegotiatingAgent with ProposalBased[Lang] with AgentHelpers[Lang] with ProposalRegister[Lang]
{
  def behaviourOnProposal: PriorityBasedBacktrackBehaviour[Lang#Proposal]
  def behaviourOnRejection: PriorityBasedBacktrackBehaviour[Lang#Rejected]
  def behaviourOnAcceptance: PriorityBasedBacktrackBehaviour[Lang#Accepted]

  /** blocking until resolved
    */
  def resolvePriorityConflict(causedBy: Message): Boolean

  trait PriorityBasedBacktrackBehaviour[Msg <: Lang#Msg]{
    def disputeOverPriorityWon(msg: Lang#Msg)
    def disputeOverPriorityLost(msg: Lang#Msg)
    def act(on: Msg)
  }

  def onProposal = {
    case prop if testMsg(prop, _ == _) =>
      if( resolvePriorityConflict(prop) ) behaviourOnProposal.disputeOverPriorityWon(prop)
      else behaviourOnProposal.disputeOverPriorityLost(prop)
    case prop if testMsg(prop, _ < _) =>
      behaviourOnProposal act prop
    case prop if testMsg(prop, _ > _) => // ignore
  }
  def onRejected = {
    case msg if testMsg(msg, _ == _) && expectingResponse(msg) =>
      if( resolvePriorityConflict(msg) ) behaviourOnRejection.disputeOverPriorityWon(msg)
      else behaviourOnRejection.disputeOverPriorityLost(msg)
    case msg if testMsg(msg, _ > _) && expectingResponse(msg) =>
      behaviourOnRejection act msg
    case msg if testMsg(msg, _ < _) || ! expectingResponse(msg) => // ignore
  }

  def onAccepted = {
    case msg if testMsg(msg, _ == _) && expectingResponse(msg) =>
      if( resolvePriorityConflict(msg) ) behaviourOnAcceptance.disputeOverPriorityWon(msg)
      else behaviourOnAcceptance.disputeOverPriorityLost(msg)
    case msg if testMsg(msg, _ > _) && expectingResponse(msg) =>
      behaviourOnAcceptance act msg
    case msg if testMsg(msg, _ < _) || ! expectingResponse(msg) => // ignore
  }

  private def testMsg(msg: Message,
                      comparePriority: (Priority, Priority) => Boolean) =
    comparePriority(msg.priority, get(msg.negotiation).priority)

}

trait ProposalRegister[Lang <: ProposalLanguage]{
  def registerProposal(msg: Lang#Proposal, to: AgentRef)
  def discardProposal(id: Message.Id)

  def expectingResponse(msg: Lang#Msg): Boolean
}