package feh.tec.agents

trait AgentHelpers[Lang <: Language]{
  self: NegotiatingAgent with SpeakingAgent[Lang] =>

  def sendToAll(neg: Negotiation, msg: Lang#Msg) = neg.scope.foreach(_ ! msg)

  protected var negotiationsCache = negotiations.map(n => n.id -> n).toMap
  def get(neg: NegotiationId) = negotiationsCache(neg)
}


trait PriorityBasedNegotiatingAgent[Lang <: ProposalLanguage]
  extends NegotiatingAgent with ProposalBased[Lang] with AgentHelpers[Lang]
{
  def behaviourOnProposal: PriorityBasedBacktrackBehaviour[Lang#Proposal]
  def behaviourOnRejection: PriorityBasedBacktrackBehaviour[Lang#Rejected]
  def behaviourOnAcceptance: PriorityBasedBacktrackBehaviour[Lang#Accepted]

  def expectingResponse(id: Message.Id): Boolean

  /** blocking until resolved
    */
  def resolvePriorityConflict(causedBy: Message): Boolean

  trait PriorityBasedBacktrackBehaviour[Msg <: Lang#Msg]{
    def disputeOverPriorityWon(msg: Lang#Msg)
    def disputeOverPriorityLost(msg: Lang#Msg)
    def act(on: Msg)
  }

  def onProposal = {
    case prop: Lang#Proposal if testMsg(prop, _.isProposal, _ == _) =>
      if( resolvePriorityConflict(prop) ) behaviourOnProposal.disputeOverPriorityWon(prop)
      else behaviourOnProposal.disputeOverPriorityLost(prop)
    case prop: Lang#Proposal if testMsg(prop, _.isProposal, _ < _) =>
      behaviourOnProposal act prop
  }
  def onRejected = {
    case msg: Lang#Rejected if testMsg(msg, _.isRejection, _ == _) && expectingResponse(msg.respondingTo) =>
      if( resolvePriorityConflict(msg) ) behaviourOnRejection.disputeOverPriorityWon(msg)
      else behaviourOnRejection.disputeOverPriorityLost(msg)
    case msg: Lang#Rejected if testMsg(msg, _.isRejection, _ > _) && expectingResponse(msg.respondingTo) =>
      behaviourOnRejection act msg
  }

  def onAccepted = {
    case msg: Lang#Accepted if testMsg(msg, _.isAcceptance, _ == _) && expectingResponse(msg.respondingTo) =>
      if( resolvePriorityConflict(msg) ) behaviourOnAcceptance.disputeOverPriorityWon(msg)
      else behaviourOnAcceptance.disputeOverPriorityLost(msg)
    case msg: Lang#Accepted if testMsg(msg, _.isAcceptance, _ > _) && expectingResponse(msg.respondingTo) =>
      behaviourOnAcceptance act msg
  }

  private def testMsg(msg: Message,
                      selectLangTest: Lang => (Any => Boolean),
                      comparePriority: (Priority, Priority) => Boolean) =
    selectLangTest(lang)(msg) && comparePriority(msg.priority, get(msg.negotiation).priority)

}

trait ProposalRegister[Lang <: ProposalLanguage]{
  def registerProposal(msg: Lang#Proposal, to: AgentRef)
  def discardProposal(id: Message.Id)

  def expectingResponse(msg: Lang#Msg): Boolean
}