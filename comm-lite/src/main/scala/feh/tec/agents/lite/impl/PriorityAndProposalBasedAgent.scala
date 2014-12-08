package feh.tec.agents.lite.impl

import akka.actor.ActorLogging
import feh.tec.agents.lite.Message.PriorityRaiseRequestId
import feh.tec.agents.lite._
import feh.tec.agents.lite
import feh.util._

import scala.collection.mutable

/** template dor generation */
trait PriorityAndProposalBasedAgent[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityProposalBasedAgent[Lang]
  with DynamicScopeSupport[Lang]
  with SpeakingSystemSupport[Lang]
  with ConstraintsSatisfactionChecks
  with ActorLogging
{
  agent =>

  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang]

  def negotiationIds: Set[NegotiationId]

  protected def createNegotiation(id: NegotiationId): Negotiation

  lazy val negotiations = negotiationIds.map(createNegotiation)

  implicit val ref = AgentRef(Agent.Id(name, role), self)

  /* == == == == Utils == == == == */

  object myPriority{
    def isHigherThenOf(msg: Lang#Msg) = comparePriority(msg, _ > _)
    def isLowerThenOf(msg: Lang#Msg) = comparePriority(msg, _ < _)
  }

  /** (this, that) => Boolean */
  def comparePriority(msg: Lang#Msg, f: (Priority, Priority) => Boolean): Boolean = {
    val myPriority = get(msg.negotiation).currentPriority()
    f(myPriority, msg.priority)
  }


  def hasState(msg: NegotiationMessage, s: NegotiationState*) = s.contains(get(msg.negotiation).currentState())
  protected val delayedMessages = mutable.ListBuffer.empty[Lang#Msg]
  def guardDelayedMessage(msg: Lang#Msg) = delayedMessages += msg
  def resendDelayedMessages() = {
    log.debug("resendDelayedMessages " + delayedMessages)
    delayedMessages.map(msg => self.tell(msg, msg.sender.ref))
//    this.receive
    delayedMessages.clear()
  }

  def process: PartialFunction[Lang#Msg, Any] = ({
    case prop: Lang#Proposal if hasState(prop, NegotiationState.Negotiating, NegotiationState.Waiting)   => onProposal lift prop
    case resp: Lang#Acceptance if hasState(resp, NegotiationState.Negotiating) => onAcceptance lift resp
    case resp: Lang#Rejection if hasState(resp, NegotiationState.Negotiating)  => onRejection lift resp
    case msg if hasState(msg, NegotiationState.Initializing, NegotiationState.Initialized, NegotiationState.Starting) => guardDelayedMessage(msg)
  }: PartialFunction[Lang#Msg, Any])
    .orElse(moreProcess).orElse{case x => unhandled(x)}

  def moreProcess: PartialFunction[Lang#Msg, Any]

  def respond(msg: Lang#Msg) = {
    val sender = Option(currentMessage).getOrThrow("cannot respond: no incoming message registered").sender
    sender ! msg
  }

  implicit class SatisfiesConstraintsWrapper(proposal: Lang#Proposal){
    def satisfiesConstraints: Boolean = agent.satisfiesConstraints(proposal.negotiation, proposal.get)
  }

}

object PriorityAndProposalBasedAgent{
  trait NegotiatesPriority[Lang <: Language.ProposalBased with Language.HasPriorityNegotiation]
    extends PriorityAndProposalBasedAgent[Lang] with lite.NegotiatesPriority[Lang]
  {

    override def process: PartialFunction[Lang#Msg, Any] = super.process orElse{
      case priority: Lang#Priority if hasState(priority, NegotiationState.Negotiating, NegotiationState.NegotiatingPriority) =>
        priorityNegotiationHandler.process(priority)
    }

    def requestPriorityRaise(neg: NegotiationId): Lang#PriorityRaiseRequest = {
      val req = priorityNegotiationHandler.start(neg)
      sendToAll(req)
      req
    }
  }
}

/*
trait PriorityNegotiationHandlerImpl[Lang <: Language.ProposalBased with Language.HasPriority] extends PriorityNegotiationHandler[Lang]
{
  protected def get: NegotiationId => AbstractNegotiation
  protected def sendAll: Lang#Msg => Unit

  protected lazy val requests = mutable.HashMap.empty[PriorityRaiseRequestId, mutable.HashMap[AgentRef, Option[Lang#PriorityRaiseRequest]]]
  protected lazy val confirmations = mutable.HashMap.empty[PriorityRaiseRequestId, mutable.HashMap[AgentRef, Option[Lang#PriorityRaiseResponse]]]
  protected def clear(id: PriorityRaiseRequestId) = {
    requests -= id
    confirmations -= id
  }
  protected def requestsMap(neg: NegotiationId) = mutable.HashMap(get(neg).scope().toSeq.zipMap(_ => Option.empty[Lang#PriorityRaiseRequest]): _*)
  def allRequests(id: PriorityRaiseRequestId): Boolean = requests.get(id).exists(_.forall(_._2.isDefined))

//  def process = {
//    case req: Lang#PriorityRaiseRequest =>
//      requests.getOrElseUpdate(req.id, requestsMap(req.negotiation))(req.sender) = Some(req)
//      if(allRequests(req.id)) sendAll(decide(requests(req.id).toMap.mapValues(_.get)).asInstanceOf[Lang#Msg])
//  }
}
*/
