package feh.tec.agents.light.impl

import akka.actor.ActorLogging
import feh.tec.agents.light.Message.PriorityRaiseRequestId
import feh.tec.agents.light._
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
    log.debug(s"comparePriority: msg=$msg")
    val negId = msg.negotiation
    log.debug(s"comparePriority: negId=$negId")
    val neg = get(negId)
    val priority = neg.currentPriority()
    log.debug(s"comparePriority: neg=$neg, priority=$priority")
    f(priority, msg.priority)
  }


  def process: PartialFunction[Lang#Msg, Any] = {
    case prop: Lang#Proposal   => onProposal lift prop
    case resp: Lang#Acceptance => onAcceptance lift resp
    case resp: Lang#Rejection  => onRejection lift resp
    case priority: Lang#Priority => priorityNegotiationHandler.process(priority)
  }

  def requestPriorityRaise(neg: NegotiationId): Lang#PriorityRaiseRequest = priorityNegotiationHandler.start(neg)

  def respond(msg: Lang#Msg) = {
    val sender = Option(currentMessage).getOrThrow("cannot respond: no incoming message registered").sender
    sender ! msg
  }

  implicit class SatisfiesConstraintsWrapper(proposal: Lang#Proposal){
    def satisfiesConstraints: Boolean = agent.satisfiesConstraints(proposal.negotiation, proposal.get)
  }

}

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

  def process = {
    case req: Lang#PriorityRaiseRequest =>
      requests.getOrElseUpdate(req.id, requestsMap(req.negotiation))(req.sender) = Some(req)
      if(allRequests(req.id)) sendAll(decide(requests(req.id).toMap.mapValues(_.get)).asInstanceOf[Lang#Msg])
  }
}
