package feh.tec.agents.lite

import feh.tec.agents.lite.DomainIteratorBuilder.LinkedDomainIterator
import feh.tec.agents.lite.Fallback.{FallbackRequest, FallbackState}
import feh.tec.agents.lite.Message.ProposalId
import feh.tec.agents.lite.impl.PriorityAndProposalBasedAgent
import feh.tec.agents.lite.impl.agent.create
import feh.tec.agents.lite.impl.agent.create.SpecExt
import feh.tec.agents.lite.impl.spec.{ChangingIssuesSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.lite.spec.{AgentOverride, AgentSpecification, RequiresDistinctPriority}
import feh.util._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.reflect.ClassTag

object QueenSpec{
  def apply() = new QueenSpec

  type Lang = create.PPI.Lang with Language.NegotiatesIssues
  type Agent =
    create.PPI.Ag[Lang]
      with FailedConfigurationsChecks[Lang]
      with FailedPartialSolutionsChecks[Lang]
      with ResponseDelay[Lang]
      with SortResending[Lang]
      with NegotiationSupport
    {
      type Negotiation <: Negotiation.HasProposal[Lang] with Negotiation.HasPriority with Negotiation.ChangingIssues with Negotiation.HasIterators
    }
}

import feh.tec.agents.lite.QueenSpec._

trait SortResending[Lang <: Language.ProposalBased with Language.HasPriority] extends PriorityAndProposalBasedAgent[Lang] with AgentOverride{
  override def resendDelayedMessages() = {
    delayedMessages
      .sortBy{
        case m: Fallback.IWillMove => 3
        case f: Fallback.FallbackRequest => 2
        case _ => 1
      }
      .map(msg => self.tell(msg, msg.sender.ref))
    delayedMessages.clear()
  }
}

trait PartialSolutionSearchSpec extends ChangingIssuesSpec[Agent, Lang] with FallbackSpec[Agent, Lang] {
  self: PriorityAndProposalBasedAgentSpec[Agent, Lang] with RequiresDistinctPriority with SpecExt[Agent] with AgentSpecification.Iterating[Agent, Lang] =>

  def confirmAllWaitingDelay: FiniteDuration

  private val _partialSolutions = mutable.Map.empty[NegotiationId, mutable.ListBuffer[PartialSolution]]
  /** Last goes first */
  def usingPartialSolutions = _partialSolutions
  def addPartialSolution(ps: PartialSolution) =
    ps +=: _partialSolutions.getOrElseUpdate(ps.negotiation, mutable.ListBuffer.empty[PartialSolution])
  def rmLastPartialSolution(neg: NegotiationId): Unit = _partialSolutions.get(neg).foreach(_.remove(0))

  /** Used only by top-priority */
  private lazy val partialSolutionReg = mutable.HashMap.empty[NegotiationId, mutable.HashMap[AgentRef, Option[(Priority, Map[Var, Any])]]]
  def currentPartialSolutionOpt(neg: NegotiationId)(implicit ag: Agent) =
    if(partialSolutionReg.get(neg).exists(_.forall(_._2.isDefined)))
      Some(
        PartialSolution(
          neg,
          ag.get(neg).currentIssues().toSet,
          partialSolutionReg(neg).map(_._2.get).toMap + (ag.get(neg).currentPriority() -> ag.get(neg).currentValues())
        )
      )
    else None

  def regAggregateReq(req: Message.IssuesRequest)(implicit ag: Agent): Unit =
    if(req.req.isAggregation){
      val neg = ag.get(req.negotiation)
      if(req.myValues.keySet == neg.currentIssues().toSet){
        val psForNeg = partialSolutionReg.getOrElseUpdate(
          req.negotiation,
          mutable.HashMap(neg.scope().toSeq.zipMap(_ => Option.empty[(Priority, Map[Var, Any])]): _*)
        )
        psForNeg += req.sender -> Some(req.priority -> req.myValues)
      }
    }
    else unregPartialSolution(req.negotiation, req.sender)

  def unregPartialSolution(neg: NegotiationId, ag: AgentRef): Unit = partialSolutionReg.get(neg).foreach(_(ag) = None)
  def clearPartialSolution(neg: NegotiationId): Unit = partialSolutionReg -= neg

  def updateIteratorOnIssuesChange(neg: Agent#Negotiation)
                                  (implicit ag: Agent) = neg.currentIterator update ag.newIterator(neg.id)

  def addIssue(to: Agent#Negotiation, next: Var) = {
    assert(!to.currentIssues().contains(next))
    to.currentIssues update next +: to.currentIssues()
  }

  def rmIssue(from: Agent#Negotiation, issue: Var) = {
    from.currentIssues update from.currentIssues().ensuringNot(_.contains(issue)).filterNot(_ == issue)
  }

  def rmLastIssue(from: Agent#Negotiation)(implicit ag: Agent) = {
    val varToRemove :: theRest = from.currentIssues()

    if(theRest.isEmpty) ag.failed(from.id, "theRest.isEmpty")

    from.currentIssues update theRest
    varToRemove
  }

  newIterator <:= {
    implicit ag => {
      negId =>
        val neg = ag.get(negId)
        val issuesOrder = neg.currentIssues().toList
        val fixedIssuesValues = neg.currentValues()
        val dItByVar = issuesOrder match {
          case changing :: statics =>
            assert(statics.toSet subsetOf fixedIssuesValues.keySet)
            val cIt = statics.map{
              issue =>
                DomainIteratorBuilder.constant[Var#Domain, Var#Tpe](fixedIssuesValues(issue).asInstanceOf[Var#Tpe], 1)
            }
            issuesOrder zip (ag.domainIterators(changing) :: cIt)
        }

        val it = DomainIteratorBuilder overSeq dItByVar.map(_._2)
        val vars = dItByVar.map(_._1)
        val i2i: Seq[Any] => Map[Var, Any] = seq => {
          assert(vars.length == seq.length)
          vars.zip(seq).toMap
        }
        val domains = dItByVar.map(_._1.domain)
        val dit = it.apply(domains).map(i2i)
        val dit2 = it.apply(domains).map(i2i)
        new LinkedDomainIterator(dit.toStream)
    }
  }

  override def hasNothingToProposeWhileTopPriority(negId: NegotiationId)(implicit ag: Agent) = {
    ag.guardFailedPartialSolution(currentPartialSolutionOpt(negId).getOrThrow(s"No partial solution exists for $negId"))

    val neg = ag.get(negId)
    val toRemove = rmLastIssue(neg)

    ag.sendToAll(Message.IssuesDemand(negId, Message.IssueChange.Remove(toRemove), neg.currentPriority())(ag.ref))
    updateIteratorOnIssuesChange(neg)
    ag.sendToAll(ag.setNextProposal(negId))
  }

  case class ConfirmAllWaiting(neg: NegotiationId) extends UserMessage

  onIssueRequest <:= {
    implicit ag => {
      case req if maxPriority(req.negotiation).exists(_._1 == ag.ref) =>
        regAggregateReq(req)

        val neg = ag.get(req.negotiation)
        val psOpt = currentPartialSolutionOpt(req.negotiation)

        if(req.req.isAggregation){
          psOpt map addPartialSolution

          if(psOpt.nonEmpty || neg.currentIssues().isEmpty){
            val issueToAdd = neg.issues diff neg.currentIssues().toSet

            if(issueToAdd.isEmpty) {
              ag.context.system.scheduler.scheduleOnce(confirmAllWaitingDelay, ag.self, ConfirmAllWaiting(neg.id))(ag.context.dispatcher)
            }
            else {
              val next = issueToAdd.randomChoose

              addIssue(neg, next)
              val d = Message.IssuesDemand(req.negotiation, Message.IssueChange.Add(next), neg.currentPriority())(ag.ref)
              ag.sendToAll(d)

              updateIteratorOnIssuesChange(neg)
              clearPartialSolution(neg.id)
              Thread.sleep(ag.responseDelay.toMillis / 2)

              if(neg.currentState() != NegotiationState.Negotiating) neg.currentState update NegotiationState.Negotiating
              val p = ag.setNextProposal(req.negotiation)
              ag.sendToAll(p)
            }
          }
        }
//        else {
//          clearPartialSolution(neg.id)
//          unregPartialSolution(req.negotiation, req.sender)
//        }

      case _ => // do nothing
    }
  }

  processUserMessage andThen{
    ag =>
      overridden =>
        overridden(ag) orElse
          {
            case ConfirmAllWaiting(neg) =>
              def diff = ag.get(neg).issues diff ag.get(neg).currentIssues().toSet
              val psOpt = currentPartialSolutionOpt(neg)(ag)

              if(psOpt.nonEmpty && diff.isEmpty){
                val ps = psOpt.get
                val sol = ps
                  .ensuring(_.issues == ag.get(neg).issues, "The known solution is only partial: " + ps)
                  .values.ensuring(_.size == ag.get(neg).scope().size + 1, "The solution isn't known for some agents: " + ps)
                ag.finished(neg, Solution(sol))
              }
          }
  }

  onIssueResponse <:= {
    implicit ag => {
      case resp if ag.myPriority isLowerThenOf resp => // do as it says
        val neg = ag.get(resp.negotiation)
        resp.req match {
          case Message.IssueChange.Add(issue)    => addIssue(neg, issue)
          case Message.IssueChange.Remove(issue) => rmIssue(neg, issue)
        }
        updateIteratorOnIssuesChange(neg)
        if(neg.currentState() != NegotiationState.Negotiating) neg.currentState update NegotiationState.Negotiating
        ag.sendToAll(ag.setNextProposal(neg.id))
      case resp =>
        val neg = ag.get(resp.negotiation)
      // do nothing
    }
  }

  def waitingStateSet(negId: NegotiationId)(implicit ag: Agent) = {
    val neg = ag.get(negId)
    ag.sendToAll(Message.IssuesRequest(negId, Message.IssueChange.Add(), neg.currentPriority(), neg.currentValues())(ag.ref))
  }

  def addOnStateChangeHookForIssueNeg(negId: NegotiationId)(implicit ag: Agent): Unit =
    ag.get(negId).VarUpdateHooks.addHook("issue-negotiation-waiting-hook",
      _.get("state").withFilter(_._2 == NegotiationState.Waiting).foreach(
        _ => waitingStateSet(negId)
      )
    )

  def requestIssueRemoval(negId: NegotiationId)(implicit ag: Agent) ={
    val neg = ag.get(negId)
    ag.sendToAll(Message.IssuesRequest(negId, Message.IssueChange.Remove(), neg.currentPriority(), neg.currentValues())(ag.ref))
  }

  override def sendFallbackRequest(negId: NegotiationId)(implicit ag: Agent) = {
    super.sendFallbackRequest(negId)
    ag.log.info("sendFallbackRequest: requestIssueRemoval")
    requestIssueRemoval(negId)
  }


  override def onFallbackRequest(req: FallbackRequest)(implicit ag: Agent) = {
    super.onFallbackRequest(req)
    if(ag.myPriority isLowerThenOf req){
      if(pausedByFallbackRequest.nonEmpty) ag.guardDelayedMessage(req)
      else {
        val neg = ag.get(req.negotiation)
        neg.currentState update FallbackState
        pausedByFallbackRequest = Some(req.id)
      }
    }
  }

  override protected def respondIWillMove(req: FallbackRequest, myPriority: Priority)(implicit ag: Agent) = {
    ag.sendToAll(Fallback.IWillMove(req.negotiation, myPriority, req.id)(ag.ref))
    requestIssueRemoval(req.negotiation)
  }

}

class QueenSpec(implicit val agentTag: ClassTag[Agent]) extends create.PPI.DynIssuesSpec[Agent, Lang]
  with RequiresDistinctPriority with PartialSolutionSearchSpec
{

  def confirmAllWaitingDelay: FiniteDuration = 20 millis span

  initialize before {
    ag =>
      ag.Reporting.Messages = false
      ag.Reporting.States = false
  }

  initialize after {
    ag => _ =>
      ag.negotiations.map(_.id).foreach(addOnStateChangeHookForIssueNeg(_)(ag))
      ag.log.info("initialized")
  }

  start andThen {
    ag =>
      overridden =>
        import ag._
        overridden(ag)
        negotiations.foreach {
          neg =>
            if(neg.currentIssues().isEmpty)
              sendToAll(Message.IssuesRequest(neg.id, Message.IssueChange.Add(), neg.currentPriority(), Map()))
            else {
              if(neg.currentIterator.raw.isEmpty) neg.currentIterator update ag.newIterator(neg.id)
              val prop = ag.setNextProposal(neg.id)
              neg.currentState update NegotiationState.Negotiating
              sendToAll(prop)
            }
        }
  }

  def respondToProposal(msg: Message.Proposal)(implicit ag: Agent) = {
    import ag._
    msg match{
      case Message.Proposal(propId, negId, _, values) =>
        val neg = get(negId)

        supervisorProposalProcessing(msg)

        def accepted = Message.Accepted(negId, propId, neg.currentPriority(), neg.currentValues(), neg.currentState() == NegotiationState.Waiting)
        def rejected = Message.Rejected(negId, propId, neg.currentPriority(), get(negId).currentValues())

        val response = // #01
          if(neg.currentState() == NegotiationState.Waiting)
            if(msg.satisfiesConstraints) accepted else rejected
          else accepted
        respond(response)
    }
  }


  onProposal <:= {
    implicit ag =>
      import ag._
      {
        case msg if(myPriority isLowerThenOf  msg) && !hasState(msg, FallbackState) =>
          if(!msg.satisfiesConstraints) {
            get(msg.negotiation).currentState update NegotiationState.Negotiating
            requestIssueRemoval(msg.negotiation)
            sendToAll(ag.setNextProposal(msg.negotiation))
          }
          respondToProposal(msg)
        case msg if myPriority isLowerThenOf  msg => respondToProposal(msg)
        case msg if myPriority isHigherThenOf msg => respondToProposal(msg)
      }
  }

  // #01
  override def whenProposalAccepted(neg: NegotiationId, pid: ProposalId)(implicit ag: Agent) = {
    super.whenProposalAccepted(neg, pid)
    ag.sendToAll(ag.get(neg).currentProposal()) // to move those that accepted their sate by error
  }

  def rejected(neg: NegotiationId)(implicit ag: Agent) = {
    ag.sendToAll(ag.setNextProposal(neg))
  }

  onRejection <:= {
    implicit ag => {
      case msg if msg.respondingTo != ag.get(msg.negotiation).currentProposal().id => //ignore
      case msg if ag.myPriority isHigherThenOf msg =>
        //mark as accepted
        setProposalAcceptance(msg, v = true)
        whenProposalAccepted(msg)
      case msg if ag.myPriority isLowerThenOf  msg =>
        setProposalAcceptance(msg, v = false)
        rejected(msg.negotiation)
    }
  }

  onAcceptance <:= {
    implicit ag => {
      case msg if msg.respondingTo != ag.get(msg.negotiation).currentProposal().id => //ignore
      case msg =>
        setProposalAcceptance(msg, v = true)
        whenProposalAccepted(msg)
/*
        ag.repeatingAFailure(msg) match {
          case Some(true) =>
//            ag.log.info("rejecting repeating error" + knownConfiguration(msg.negotiation, msg.respondingTo))
            rejected(msg.negotiation)
          case Some(false) | None =>
            setProposalAcceptance(msg, v = true)
            whenProposalAccepted(msg)
//          case None => // wait for more
        }
*/
    }
  }

}