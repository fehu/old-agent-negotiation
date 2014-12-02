package feh.tec.agents.lite

import java.util.UUID

import feh.tec.agents.lite.Fallback
import feh.tec.agents.lite.Fallback.{FallbackRequest, FallbackState}
import feh.tec.agents.lite.impl.agent.create
import feh.tec.agents.lite.impl.agent.create.SpecExt
import feh.tec.agents.lite.impl.spec.{PriorityAndProposalBasedAgentSpec, ChangingIssuesSpec}
import feh.tec.agents.lite.spec.{AgentSpecification, RequiresDistinctPriority}
import feh.util._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.reflect.ClassTag

object QueenSpec{
  def apply() = new QueenSpec

  type Lang = create.PPI.Lang with Language.NegotiatesIssues
  type Agent = create.PPI.Ag[Lang] with FailedConfigurationsChecks[Lang] with FailedPartialSolutionsChecks[Lang]{
    type Negotiation <: Negotiation.HasProposal[Lang] with Negotiation.HasPriority with Negotiation.ChangingIssues with Negotiation.HasIterators
  }
}

import feh.tec.agents.lite.QueenSpec._

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

  def regAggregateReq(req: Message.IssuesRequest)(implicit ag: Agent): Unit = {
    val neg = ag.get(req.negotiation)
    ag.log.debug(s"req.myValues.keySet = ${req.myValues.keySet}, neg.currentIssues()=${neg.currentIssues()}, neg.currentValues() = ${neg.currentValues()}")
    if(req.myValues.keySet == neg.currentIssues().toSet){
      val psForNeg = partialSolutionReg.getOrElseUpdate(
        req.negotiation,
        mutable.HashMap(neg.scope().toSeq.zipMap(_ => Option.empty[(Priority, Map[Var, Any])]): _*)
      )
//      assert(psForNeg(req.sender).isEmpty, s"psForNeg(req.sender)=${psForNeg(req.sender)}")
      psForNeg += req.sender -> Some(req.priority -> req.myValues)
      ag.log.debug(s"psForNeg=$psForNeg")
    }
  }
  def unregPartialSolution(neg: NegotiationId, ag: AgentRef): Unit = partialSolutionReg.get(neg).foreach(_ += ag -> None)
  def clearPartialSolution(neg: NegotiationId): Unit = partialSolutionReg -= neg

  def updateIteratorOnIssuesChange(neg: Agent#Negotiation)
                                  (implicit ag: Agent) = {
    val issues = neg.currentIssues().toSet
    val itOpt = neg.currentIterators().get(issues)
    val it = itOpt.getOrElse{
        val i = ag.newIterator(neg.id)
        neg.currentIterators update (neg.currentIterators() + (issues -> i))
        i
      }
    neg.currentIterator update it
  }

  def addIssue(to: Agent#Negotiation, next: Var) = {
    assert(!to.currentIssues().contains(next))
    to.currentIssues update next +: to.currentIssues()
  }

  def rmIssue(from: Agent#Negotiation, issue: Var) = {
    from.currentIssues update from.currentIssues().ensuringNot(_.contains(issue)).filterNot(_ == issue)
  }

  def rmLastIssue(from: Agent#Negotiation) = {
    val varToRemove :: theRest = from.currentIssues()

    if(theRest.isEmpty) sys.error("Negotiation failed")

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
            ag.log.debug(s"issuesOrder=$issuesOrder")
            ag.log.debug(s"changing=$changing, fixedIssuesValues=$fixedIssuesValues")
            ag.log.debug(s"statics.toSet=${statics.toSet}, fixedIssuesValues.keySet=${fixedIssuesValues.keySet}")
            assert(statics.toSet subsetOf fixedIssuesValues.keySet)
            val cIt = statics.map{
              issue =>
                ag.log.debug(s"DomainIterator constant(${fixedIssuesValues(issue)}, 1)")
                DomainIteratorBuilder
                  .constant[Var#Domain, Var#Tpe](fixedIssuesValues(issue).asInstanceOf[Var#Tpe], 1)
            }
            issuesOrder zip (ag.domainIterators(changing) :: cIt)
        }
          //ag.varsByNeg(negId).toSeq.zipMap(ag.domainIterators)
        val it = DomainIteratorBuilder overSeq dItByVar.map(_._2)
        val vars = dItByVar.map(_._1)
        val i2i: Seq[Any] => Map[Var, Any] = seq => {
          assert(vars.length == seq.length)
          vars.zip(seq).toMap
        }
        val domains = dItByVar.map(_._1.domain)
        val dit = it.apply(domains).map(i2i)
        val dit2 = it.apply(domains).map(i2i)
        ag.log.debug("dit2 = " + dit2.toList.mkString("\n"))
        dit
    }
  }

  override def hasNothingToProposeWhileTopPriority(negId: NegotiationId)(implicit ag: Agent) = {
    ag.guardFailedPartialSolution(currentPartialSolutionOpt(negId).getOrThrow(s"No partial solution exists for $negId"))

    val neg = ag.get(negId)

    val toRemove = rmLastIssue(neg)
    ag.sendToAll(Message.IssuesDemand(negId, Message.IssueChange.Remove(toRemove), neg.currentPriority())(ag.ref))

//    neg.currentIterator update neg.currentIterators().getOrElse(theRest.toSet, ag.newIterator(negId))
    updateIteratorOnIssuesChange(neg)
//    Thread.sleep(100)
    ag.sendToAll(ag.setNextProposal(negId))
  }

  case class ConfirmAllWaiting(neg: NegotiationId) extends UserMessage

  onIssueRequest <:= {
    implicit ag => {
      case req if maxPriority(req.negotiation).exists(_._1 == ag.ref) =>
        regAggregateReq(req)

        val neg = ag.get(req.negotiation)
        val psOpt = currentPartialSolutionOpt(req.negotiation)

        ag.log.debug(s"onIssueRequest: negID = ${neg.id} partialSolution = ${partialSolutionReg.get(neg.id)}; //$partialSolutionReg")
        ag.log.debug("onIssueRequest: req " + req)

        if(req.req.isAggregation){
          psOpt map addPartialSolution

          if(psOpt.nonEmpty || neg.currentIssues().isEmpty){
            val issueToAdd = neg.issues diff neg.currentIssues().toSet

            if(issueToAdd.isEmpty) {
              ag.context.system.scheduler.scheduleOnce(confirmAllWaitingDelay, ag.self, ConfirmAllWaiting(neg.id))(ag.context.dispatcher)
//              sys.error("Negotiation finished: " + psOpt + "\nissueToAdd = " + issueToAdd)
            } // todo
            else {
              val next = issueToAdd.randomChoose

              addIssue(neg, next)
              val d = Message.IssuesDemand(req.negotiation, Message.IssueChange.Add(next), neg.currentPriority())(ag.ref)
              ag.log.debug("onIssueRequest " + d)
              ag.sendToAll(d)

              updateIteratorOnIssuesChange(neg)
              ag.log.debug("updateIteratorOnIssuesChange")
              clearPartialSolution(neg.id)
              Thread.sleep(100)

              if(neg.currentState() != NegotiationState.Negotiating) neg.currentState update NegotiationState.Negotiating
              val p = ag.setNextProposal(req.negotiation)
              ag.log.debug("onIssueRequest: sending next proposal " + p)
              ag.sendToAll(p)
            }
          }
        }
        else {
          clearPartialSolution(neg.id)
//          unregPartialSolution(req.negotiation, req.sender)
        }

//        assert(, s"psOpt=$psOpt, neg.currentIssues()=${neg.currentIssues()}")
      case _ => // do nothing
        ag.log.debug("onIssueRequest: do nothing")
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
              val f = ag.failedPartialSolutions(neg)
              if(psOpt.nonEmpty && diff.isEmpty)
                sys.error(s"Negotiation finished: ${psOpt.get.values.values}\nfails: $f")
          }
  }

  onIssueResponse <:= {
    implicit ag => {
      case resp if ag.myPriority isLowerThenOf resp => // do as it says
        ag.log.debug("onIssueResponse " + resp)
        val neg = ag.get(resp.negotiation)
        resp.req match {
          case Message.IssueChange.Add(issue)    => addIssue(neg, issue)
          case Message.IssueChange.Remove(issue) => rmIssue(neg, issue)
        }
        updateIteratorOnIssuesChange(neg)
        if(neg.currentState() != NegotiationState.Negotiating) neg.currentState update NegotiationState.Negotiating
        //unregPartialSolution(resp.negotiation, resp.sender)
//        Thread.sleep(100)
        ag.sendToAll(ag.setNextProposal(neg.id))
      case resp =>
        val neg = ag.get(resp.negotiation)
        if(neg.currentState() == NegotiationState.Waiting)
          ag.sendToAll(Message.IssuesRequest(resp.negotiation, Message.IssueChange.Add(), neg.currentPriority(), neg.currentValues())(ag.ref))
      // do nothing
    }
  }

  def waitingStateSet(negId: NegotiationId)(implicit ag: Agent) = {
    val neg = ag.get(negId)
    ag.log.debug(s"waitingState set for $negId")
    ag.sendToAll(Message.IssuesRequest(negId, Message.IssueChange.Add(), neg.currentPriority(), neg.currentValues())(ag.ref))
  }

  def addOnStateChangeHookForIssueNeg(negId: NegotiationId)(implicit ag: Agent): Unit =
    ag.get(negId).VarUpdateHooks.addHook("issue-negotiation-waiting-hook",
      _.get("state").withFilter(_._2 == NegotiationState.Waiting).foreach(
        _ => waitingStateSet(negId)
      )
    )

  override def sendFallbackRequest(negId: NegotiationId)(implicit ag: Agent) = {
    super.sendFallbackRequest(negId)
    val neg = ag.get(negId)
    ag.sendToAll(Message.IssuesRequest(negId, Message.IssueChange.Remove(), neg.currentPriority(), neg.currentValues())(ag.ref))
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

  override protected def respondIWillMove(req: FallbackRequest, myPriority: Priority)(implicit ag: Agent) =
  {
//    Thread.sleep(50)
    ag.sendToAll(Fallback.IWillMove(req.negotiation, myPriority, req.id)(ag.ref))
  }
}

class QueenSpec(implicit val agentTag: ClassTag[Agent]) extends create.PPI.DynIssuesSpec[Agent, Lang]
  with RequiresDistinctPriority with PartialSolutionSearchSpec
{

  def confirmAllWaitingDelay: FiniteDuration = 400 millis span

  initialize before {
    ag =>
//      ag.Reporting.Messages = false
//      ag.Reporting.States = false
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
            log.debug(s"currentIssues=${neg.currentIssues()}") // currentIterator=${neg.currentIterator()}\n
            if(neg.currentIssues().isEmpty){
              log.debug("spamming IssuesRequest in the begining")
              sendToAll(Message.IssuesRequest(neg.id, Message.IssueChange.Add(), neg.currentPriority(), Map()))
            }
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
        val response =
          if(msg.satisfiesConstraints)
            Message.Accepted(negId, propId, neg.currentPriority(), neg.currentValues(), neg.currentState() == NegotiationState.Waiting)
          else Message.Rejected(negId, propId, neg.currentPriority(), get(negId).currentValues())
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
            sendToAll(ag.setNextProposal(msg.negotiation))
          }
          respondToProposal(msg)
        case msg if myPriority isLowerThenOf  msg => respondToProposal(msg)
        case msg if myPriority isHigherThenOf msg => respondToProposal(msg)
      }
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