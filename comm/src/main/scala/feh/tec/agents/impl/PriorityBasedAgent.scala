package feh.tec.agents.impl

import akka.pattern.ask
import akka.util.Timeout
import feh.tec.agents.service.ConflictResolver.{ConflictResolved, ResolveConflict}
import feh.tec.agents._

import scala.concurrent.Await
import scala.util.{Failure, Success}

/** Intended to be used with views
 */
trait PriorityBasedAgent[Lang <: ProposalLanguage] extends Agent[Lang]
  with PriorityBasedNegotiatingAgent[Lang] with ExternalViewSupport
{
  def conflictResolver: AgentRef
  def conflictResolveTimeout: Timeout

  /** test if accept the proposal */
  def accept_?(prop: Lang#Proposal): Boolean

  /** check your views and act accordingly */
  def checkConstraints(negId: NegotiationId)
  
  /** blocking until resolved
    */
  def resolvePriorityConflict(causedBy: Message) = {
    causedBy.sender ! priorityConflictMsg(causedBy)
    askConflictResolver(causedBy)
  }

  protected def askConflictResolver(causedBy: Message) ={
    val req = ResolveConflict("priority", causedBy.negotiation, causedBy.sender)
    implicit def timeout = conflictResolveTimeout
    Await.ready(conflictResolver.ref ? req, timeout.duration*2).value.get match {
      case Success(ConflictResolved(req.id, won, _)) => won
//      case Failure(_: TimeoutException) => true
    }

  }

  protected def priorityConflictMsg(causedBy: Message): Lang#Conflict
  protected def isPriorityConflict(msg: Lang#Msg): Boolean


  override def process = super.process orElse{
    case priorityConflict if isPriorityConflict(priorityConflict) =>
      askConflictResolver(priorityConflict)
  }

  protected def spamProposal(neg: ANegotiation)

  class OnProposalBehaviour extends PriorityBasedOnProposalBehaviour[Lang#Proposal]{
    def disputeOverPriorityWon(msg: Lang#Msg) = {
      risePriority(msg.negotiation)
      act(msg.asInstanceOf[Lang#Proposal])
    }
    def disputeOverPriorityLost(msg: Lang#Msg) = {} // do nothing
    def act(prop: Lang#Proposal) =
      prop.sender ! (if(accept_?(prop)) createAccepted(prop.negotiation) else createRejected(prop.negotiation))

    def reassessTheProposal(msg: Lang#Msg) = spamProposal(get(msg.negotiation))
  }

  lazy val behaviourOnProposal = new OnProposalBehaviour

  lazy val behaviourOnRejection = new PriorityBasedBacktrackBehaviour[Lang#Rejected] {
    def disputeOverPriorityWon(msg: Lang#Msg) = {
      risePriority(msg.negotiation)
      spamProposal(get(msg.negotiation))
//      checkConstraints(msg.negotiation)
    }
    def disputeOverPriorityLost(msg: Lang#Msg) = checkConstraints(msg.negotiation)
    def act(on: Lang#Rejected) = checkConstraints(on.negotiation)
  }

  lazy val behaviourOnAcceptance = new PriorityBasedBacktrackBehaviour[Lang#Accepted] {
    def disputeOverPriorityWon(msg: Lang#Msg) = {
      risePriority(msg.negotiation)
      spamProposal(get(msg.negotiation))
//      checkConstraints(msg.negotiation)
    }
    def disputeOverPriorityLost(msg: Lang#Msg) = checkConstraints(msg.negotiation)
    def act(on: Lang#Accepted) = checkConstraints(on.negotiation)
  }

  protected def risePriority(negId: NegotiationId) = {
    val neg = get(negId)
    neg.currentPriority = new Priority(neg.currentPriority.get + 1)
    updateProposal(neg)
  }
  protected def updateProposal(neg: ANegotiation): Lang#Proposal
}

trait PriorityBasedAgentViews extends ExternalViewSupport{
  self: PriorityBasedAgent[_] =>

  def constraintsSatisfactions: view.ConstraintsSatisfactionWithPriority
  def proposalSatisfaction: view.Constraints

  def externalViews = (constraintsSatisfactions: ExternalView) :: Nil
  def filterIncoming = _ => true
}
