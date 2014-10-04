package feh.tec.agents.impl.agent

import java.util.UUID

import akka.actor.{Cancellable, ActorLogging}
import akka.util.Timeout
import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents._
import AgentCreation.NegotiationInit
import feh.tec.agents.impl.Agent.{AgentReporting, Status}
import feh.tec.agents.impl.ProposalEngine.{SharingKnowledge, MySolutionValues}
import feh.tec.agents.impl._
import feh.tec.agents
import feh.tec.agents.impl.view.{Priority, ExternalConstraints, Constraints, ConstraintsSatisfactionWithPriority}
import feh.util._

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration


object PriorityBased{
  protected case object CheckConstraints extends LocalSystemMessage
}

trait PriorityBased[Lang <: ProposalLanguage] extends PriorityBasedAgent[Lang]
  with impl.Agent.ProposalRegistering[Lang]
  with ProposalEngine[Lang]
  with ProposalEngine.LearningFromMistakes[Lang]
  with PriorityBasedAgentViews
  with ViewUtils
  with ActorLogging
{


  type StateOfNegotiation <: ProposalNegotiationState[Lang] with ProposalViewState

  def constraintsFilter: Message => Boolean = _ => true
  lazy val constraintsSatisfactions = new ConstraintsSatisfactionWithPriority(
    new ExternalConstraints(lang, constraintsFilter), new Priority(constraintsFilter))

  lazy val proposalSatisfaction = new Constraints(constraints)

  def scheduleCheckConstraints(init: FiniteDuration, delay: FiniteDuration) =
    context.system.scheduler.schedule(init, delay, self, PriorityBased.CheckConstraints)(context.system.dispatcher, self)

  def constraints: Set[Constraint]
  protected implicit def issuesExtractor: IssuesExtractor[Lang]

  protected def isFailure(neg: Negotiation, weighted: Map[Option[Boolean], InUnitInterval]): Boolean

  def accept_?(prop: Lang#Proposal) =
    proposalSatisfaction.satisfy(issuesExtractor.extract(prop), get(prop.negotiation).currentValues.toMap)


  override def processSys = super.processSys orElse{
    case PriorityBased.CheckConstraints if status == Status.Working => negotiations.foreach(_.id |> checkConstraints)
    case PriorityBased.CheckConstraints =>
      checkConstraintsTimer.cancel()
      checkConstraintsTimer = null
  }


  def resetIterator(negId: NegotiationId)
  def resetIteratorIfNewTopPriority(msg: Lang#Msg) = {
    if(lastOnProposalMaxPriority.exists(msg.priority > _)) resetIterator(msg.negotiation)
    lastOnProposalMaxPriority = Some(maxPriority)
  }

  protected var lastOnProposalMaxPriority: Option[agents.Priority] = None

  override lazy val behaviourOnProposal = new OnProposalBehaviour{
    override def act(prop: Lang#Proposal) = {
      resetIteratorIfNewTopPriority(prop)
      super.act(prop)
    }

    override def disputeOverPriorityWon(msg: Lang#Msg) = {
      resetIteratorIfNewTopPriority(msg)
      super.disputeOverPriorityWon(msg)
    }

    override def disputeOverPriorityLost(msg: Lang#Msg) = {
      resetIteratorIfNewTopPriority(msg)
      super.disputeOverPriorityLost(msg)
    }

    override def reassessTheProposal(msg: Lang#Msg) = {
      resetIteratorIfNewTopPriority(msg)
      super.reassessTheProposal(msg)
    }
  }

  def maxPriority = constraintsSatisfactions.merge._2.data.map(_._2._2).maxBy(_.get)

  def checkConstraints(negId: NegotiationId): Unit = get(negId) |> {
    neg =>
      val currentProposal = neg.state.currentProposal.get
      val viewsByMsgId = constraintsSatisfactions.data.regroupSeq{
        case (_mp, _pr) =>
          // null tests
          val mp = Option(_mp)
          val pr = Option(_pr) //Message.Id, Option[Boolean]


          mp.map(_.toSeq).getOrElse(Nil).map{
            case (msgId, opt) => msgId -> (opt -> (negId -> pr.map(_._2).getOrElse(null.asInstanceOf[agents.Priority])))
          }
      }

      if(neg.currentPriority > maxPriority) {
        markUncondAccepted(neg)
        return
      }

      val weighted = viewsByMsgId.getOrElse(currentProposal.id, Map()).weight{
        case (ag, (opt, pr)) if neg.scope.contains(ag) && pr._2 > neg.currentPriority => opt
      }
      neg.state.lastWeightedProposal = Some(weighted)

//          log.info("weighted = " + weighted)
//          log.info("currentProposalDate = " + neg.state.currentProposalDate.map(_.getTime))
//          log.info("weighted = " + weighted)
//          log.info("viewsByMsgId = " + viewsByMsgId)

      if(weighted.isEmpty || weighted.map(_._2).sum.d == 0) {
        spamProposal(neg)
        return
      }

//          log.info("constraintsSatisfactions.data = " + constraintsSatisfactions.data)
//          log.info("externalConstraints.data = " + constraintsSatisfactions.merge._1.data)

      if(isFailure(neg, weighted)) externalConstraintsNotSatisfied(neg)
      else markAccepted(neg)
  }

  def externalConstraintsNotSatisfied(neg: ANegotiation) = setNextProposal(neg)
    .map( _ => spamProposal _ )
    .getOrElse( noMoreProposals _ )
    .apply(neg)

  def markAccepted(neg: ANegotiation) = {
    val old = neg.currentValuesAcceptance
    neg.currentValuesAcceptance = true
    // reassure
    if(!old) spamProposal(neg)
  }
  def markUncondAccepted(neg: ANegotiation) = {
    markAccepted(neg)
    neg.state.currentProposalUnconditionallyAccepted = true
  }
  def markedUncondAccepted(neg: ANegotiation) = neg.state.currentProposalUnconditionallyAccepted

  var checkConstraintsTimer: Cancellable = null
  def checkConstraintsRepeat: FiniteDuration

  override def startLife() = negotiations foreach {
    neg =>

//      val before = "="*3 + " BEFORE " + "="*3

//      log.info(s"$before externalViews: $externalViews")
//      log.info(s"$before failedValueConfigurations: $failedValueConfigurations")
//      log.info(s"$before proposalsWithoutResponse: $proposalsWithoutResponse")
//      log.info(s"$before neg.state.lastWeightedProposal: ${neg.state.lastWeightedProposal}")
//      log.info(s"$before neg.priority: ${neg.priority}")
//      log.info(s"$before neg.currentValue: ${neg.currentValues}")
//      log.info(s"$before neg.state.currentProposal: ${neg.state.currentProposal}")

      externalViews.foreach(_.reset())
      proposalsWithoutResponse.clear()

      neg.state.lastWeightedProposal = None
      neg.resetPriority()

      resetProposal(neg)
      spamProposal(neg)
      checkConstraintsTimer = scheduleCheckConstraints(checkConstraintsRepeat, checkConstraintsRepeat)

//      val after = "="*3 + " AFTER " + "="*3

//      log.info(s"$after externalViews: $externalViews")
//      log.info(s"$after failedValueConfigurations: $failedValueConfigurations")
//      log.info(s"$after proposalsWithoutResponse: $proposalsWithoutResponse")
//      log.info(s"$after neg.state.lastWeightedProposal: ${neg.state.lastWeightedProposal}")
//      log.info(s"$after neg.priority: ${neg.priority}")
//      log.info(s"$after neg.currentValue: ${neg.currentValues}")
//      log.info(s"$after neg.state.currentProposal: ${neg.state.currentProposal}")
  }

  def resumeLife() = negotiations foreach {
    neg =>
      spamProposal(neg)
      checkConstraintsTimer = scheduleCheckConstraints(checkConstraintsRepeat, checkConstraintsRepeat)
  }

  protected def spamProposal(neg: ANegotiation) = sendToAll(neg, neg.state.currentProposal.get)

  protected def maxPriority(negId: NegotiationId) = constraintsSatisfactions.merge._2
    .data.filter(_._2._1 == negId).map(_._2._2).maxBy(_.get)

  protected def noMoreProposals(neg: ANegotiation) = {
    log.info("noMoreProposals")
    neg.currentPriority = maxPriority(neg.id).raise()
    resetProposal(neg)
    spamProposal(neg)
  }

  def topPriority_?(neg: Negotiation) = neg.state.currentProposalUnconditionallyAccepted
}

trait PriorityBasedLearningFromMistakes[Lang <: ProposalLanguage] extends PriorityBased[Lang]
  with ProposalEngine.LearningFromMistakes[Lang]
{
  self: NegotiatingAgent with ProposalBased[Lang] with ActorLogging =>

  override def startLife() = {
    super.startLife()
    failedSolutions.foreach(_._2.clear())
  }
}

trait PriorityBasedSharingAndLearningFromFatalMistakes[Lang <: ProposalLanguage]
  extends PriorityBasedLearningFromMistakes[Lang]
  with ProposalEngine.SharingKnowledge[Lang]
  with LearningFromMistakesExtraction[Lang]
{
  self: NegotiatingAgent with PriorityBasedAgent[Lang] with ActorLogging with AgentHelpers[Lang] with AgentReporting[Lang] =>

  override def externalConstraintsNotSatisfied(neg: ANegotiation) = {
    configurationFailed(neg.id)
    super.externalConstraintsNotSatisfied(neg)
  }
}

abstract class PriorityBasedCreation[Lang <: ProposalLanguage](
             uuid: UUID,
             negotiationInit: Map[NegotiationId, NegotiationInit],
             val conflictResolver: AgentRef,
             val conflictResolveTimeout: Timeout)
  extends AgentCreation[Lang](uuid, negotiationInit) with PriorityBased[Lang] with PriorityBasedLearningFromMistakes[Lang]

object PriorityBasedCreation{
  type Interface = (UUID, Map[NegotiationId, NegotiationInit], AgentRef, Timeout)

  object Builder extends AgentBuilder[PriorityBasedCreation[_], Interface]
}