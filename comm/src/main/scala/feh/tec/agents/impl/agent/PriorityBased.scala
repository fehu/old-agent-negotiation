package feh.tec.agents.impl.agent

import java.util.UUID

import akka.actor.{Cancellable, ActorLogging}
import akka.util.Timeout
import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents._
import AgentCreation.NegotiationInit
import feh.tec.agents.impl.Agent.Status
import feh.tec.agents.impl._
import feh.tec.agents
import feh.tec.agents.impl.view.{Priority, ExternalConstraints, Constraints, ConstraintsSatisfactionWithPriority}
import feh.util._

import scala.concurrent.duration.FiniteDuration


object PriorityBased{
  protected case object CheckConstraints extends LocalSystemMessage
}

trait PriorityBased[Lang <: ProposalLanguage] extends PriorityBasedAgent[Lang]
  with impl.Agent.ProposalRegistering[Lang]
  with ProposalEngine[Lang]
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
  protected def issuesExtractor: IssuesExtractor[Lang]

  protected def isFailure(neg: Negotiation, weighted: Map[Option[Boolean], InUnitInterval]): Boolean

  def accept_?(prop: Lang#Proposal) =
    proposalSatisfaction.satisfy(issuesExtractor.extract(prop), get(prop.negotiation).currentValues.toMap)


  override def processSys = super.processSys orElse{
    case PriorityBased.CheckConstraints if status == Status.Working => negotiations.foreach(_.id |> checkConstraints)
    case PriorityBased.CheckConstraints =>
      checkConstraintsTimer.cancel()
      checkConstraintsTimer = null
  }

  def checkConstraints(negId: NegotiationId): Unit = get(negId) |> {
    neg =>
      neg.state.currentProposal map {
        currentProposal =>
          val viewsByMsgId = constraintsSatisfactions.data.regroupSeq{
            case (_mp, _pr) =>
              // null tests
              val mp = Option(_mp)
              val pr = Option(_pr) //Message.Id, Option[Boolean]


              mp.map(_.toSeq).getOrElse(Nil).map{
                case (msgId, opt) => msgId -> (opt -> (negId -> pr.map(_._2).getOrElse(null.asInstanceOf[agents.Priority])))
              }
          }

          val maxPriority = constraintsSatisfactions.merge._2.data.map(_._2._2).maxBy(_.get)

          if(neg.currentPriority > maxPriority) return

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

          if(isFailure(neg, weighted))
            setNextProposal(neg)
              .map( _ => spamProposal _ )
              .getOrElse( noMoreProposals _ )
              .apply(neg)
      }
  }

  var checkConstraintsTimer: Cancellable = null
  def checkConstraintsRepeat: FiniteDuration

  override def startLife() = negotiations foreach {
    neg =>
      resetProposal(neg)
      spamProposal(neg)
      checkConstraintsTimer = scheduleCheckConstraints(checkConstraintsRepeat, checkConstraintsRepeat)
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
    neg.currentPriority = maxPriority(neg.id).raise()
    resetProposal(neg)
    spamProposal(neg)
  }
}

abstract class PriorityBasedCreation[Lang <: ProposalLanguage](
             uuid: UUID,
             negotiationInit: Map[NegotiationId, NegotiationInit],
             val conflictResolver: AgentRef,
             val conflictResolveTimeout: Timeout)
  extends AgentCreation[Lang](uuid, negotiationInit) with PriorityBased[Lang]

object PriorityBasedCreation{
  type Interface = (UUID, Map[NegotiationId, NegotiationInit], AgentRef, Timeout)

  object Builder extends AgentBuilder[PriorityBasedCreation[_], Interface]
}