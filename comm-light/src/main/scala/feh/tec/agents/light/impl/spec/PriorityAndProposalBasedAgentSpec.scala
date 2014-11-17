package feh.tec.agents.light.impl.spec

import feh.tec.agents.light.Message.{PriorityRaiseRequestId, ProposalId}
import feh.tec.agents.light.NegotiationState.{Initialized, Starting, Stopped}
import feh.tec.agents.light._
import feh.tec.agents.light.impl.PriorityAndProposalBasedAgent
import feh.tec.agents.light.spec.AgentSpecification
import feh.util._
import scala.collection.mutable

trait PriorityAndProposalBasedAgentSpec[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]
  extends AgentSpecification.PriorityAndProposalBased[Ag, Lang]   {
  //    type BuildArgs =

  lazy val nothingToPropose = new DefBADS[NegotiationId => Unit](_ => id => sys.error(s"$id: nothing to propose"))

  lazy val beforeEachMessage = new DefDS[Lang#Msg => Unit](_ => _ => {})

  lazy val setNextProposal = new DefBADS[NegotiationId => Unit](
    implicit owner => {
      negId =>
        val neg = owner.get(negId)
        val nextVals = owner.nextValues(negId)
        owner.log.debug(s"setNextProposal for $negId, $nextVals")
        nextVals.map{ neg.currentValues.update } getOrElse owner.nothingToPropose(negId) //nothingToPropose.get.apply(negId)
        owner.updateCurrentProposal(negId)
//        neg.currentProposal update Message.Proposal(Message.ProposalId.rand, negId, neg.currentPriority(), nextVals.get)(owner.ref)
    }
  )

  //    lazy val nextValues: DefBADS[NegotiationId => Option[Map[Var, Any]]] = new DefBADS[NegotiationId => Option[Map[Var, Any]]](???) // todo

  private def noDefErr[Msg](in: String): Ag => PartialFunction[Msg, Any] = (ag: Ag) => {case _: Msg => sys.error(s"no `$in` behavior defined") }

  lazy val onProposal = new DefDS[PartialFunction[Lang#Proposal, Any]](noDefErr("onProposal"))
  lazy val onAcceptance = new DefDS[PartialFunction[Lang#Acceptance, Any]](noDefErr("onAcceptance"))
  lazy val onRejection = new DefDS[PartialFunction[Lang#Rejection, Any]](noDefErr("onRejection"))

  def proposal(negId: NegotiationId)(implicit owner: Ag) = {
      val neg = owner.get(negId)
      val pr = neg.currentPriority()
      val prop = Message.Proposal(ProposalId.rand, negId, pr, neg.currentValues())(owner.ref)
      owner.log.debug(s"created proposal(spec): $prop, neg = $neg")
      prop

  }

  lazy val updateCurrentProposal = new DefBADS[NegotiationId => Unit](
    implicit owner => {
      negId =>
        val p = proposal(negId)
        owner.log.debug("updateCurrentProposal(spec): " + p)
        owner.get(negId).currentProposal update p
    })

  lazy val requestPriorityRaise = new DefBADS[NegotiationId => Lang#PriorityRaiseRequest](
    implicit owner => id => priorityNegotiationHandler.get.start.get apply id
  )

  protected var priorityNegotiationHandlerSetup = List.empty[AgentSpecification.PriorityNegotiationHandler[Ag, Lang] => Unit]
  def priorityNegotiationHandler[R](f: AgentSpecification.PriorityNegotiationHandler[Ag, Lang] => Unit) = priorityNegotiationHandlerSetup ::= f

  protected[light] lazy val priorityNegotiationHandler = new DefDSH[AgentSpecification.PriorityNegotiationHandler[Ag, Lang]](
    implicit owner =>
      new PriorityNegotiationHandlerSpec[Ag, Lang] $$ (spec => priorityNegotiationHandlerSetup.reverse.foreach(_(spec)))
  )

  lazy val initialize = new DefBADS[Unit](_.negotiations.foreach(_.ensuring(_.scope().nonEmpty).currentState update Initialized ))
  lazy val start = new DefBADS[Unit](_.negotiations.foreach(_.currentState update Starting))
  lazy val stop =  new DefBADS[Unit](_.negotiations.foreach(_.currentState update Stopped))
  lazy val reset = new DefBADS[Unit](_ => {})

}

class PriorityNegotiationHandlerSpec[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]
  extends AgentSpecification.PriorityNegotiationHandler[Ag, Lang]
{
  protected var raiseRequestId: Option[PriorityRaiseRequestId] = None
  protected lazy val requests = mutable.HashMap.empty[AgentRef, Option[Lang#PriorityRaiseRequest]]
  protected lazy val confirmations = mutable.HashMap.empty[AgentRef, Option[Lang#PriorityRaiseResponse]]
  protected def clear() = {
    raiseRequestId = None
    requests.clear()
    confirmations.clear()
  }
  protected def emptyMap[T <: Lang#Msg](msg: T)(implicit owner: Ag) =
    mutable.HashMap((owner.get(msg.negotiation).scope() + owner.ref).toSeq.zipMap{
      case msg.sender => Some(msg)
      case _          => None
    }: _*)

  def allRequests(id: PriorityRaiseRequestId) = raiseRequestId.contains(id) && requests.forall(_._2.isDefined)
  def allConfirmations(id: PriorityRaiseRequestId) = raiseRequestId.contains(id) && confirmations.forall(_._2.isDefined)

  protected def addReqEntry(req: Lang#PriorityRaiseRequest)(implicit owner: Ag) ={
    val snd = req.sender
    owner.log.debug(s"adding priority raise request by $snd")
    // if there are already requests guarded, select the created first
    if(raiseRequestId.isEmpty || (req.id.initializedAt before raiseRequestId.get.initializedAt)){
      clear()
      raiseRequestId = Some(req.id)
      requests ++= emptyMap(req)
    }
//    else if (req.id.initializedAt == raiseRequestId.get.initializedAt && req.id.get != raiseRequestId.get.get){
//      choose the one with hash-code
//    }
    else {
//      assert(requests(req.sender).isEmpty || requests(req.sender) == Some(req), s"the request is already defined for agent ${req.sender} with ${req.id}; requests=$requests")
//      assert(requests(req.sender).isEmpty, s"the request is already defined for agent ${req.sender} with ${req.id}; requests=$requests")
      requests(req.sender) = Some(req)
    }
  }

  protected def addConfirmEntry(resp: Lang#PriorityRaiseResponse)(implicit owner: Ag) ={
    val sndr = resp.sender
    owner.log.debug(s"adding priority raise request by $sndr")
//    assert(raiseRequestId.contains(resp.respondingTo), s"the $raiseRequestId doesn't match $resp, ${resp.respondingTo}")
    if(confirmations.isEmpty) confirmations ++= emptyMap(resp)
    else {
//      assert(confirmations(sndr).isEmpty, s"the response is already defined for ${resp.sender} for ${resp.respondingTo}; confirmations=$confirmations")
      confirmations(sndr) = Some(resp)
    }
  }

  lazy val process = new DefDS[PartialFunction[Lang#Priority, Any]](
    implicit owner => {
      case req: Lang#PriorityRaiseRequest =>
        import owner.{log, ref, sendToAll}
        owner.get(req.negotiation).currentState update NegotiationState.NegotiatingPriority
        addReqEntry(req)
        log.debug(s"PriorityNegotiationHandlerSpec: process: $req by ${req.sender}")
        if(requests(ref).isEmpty) {
          log.debug("sending my priority raise request")
          sendToAll(start.get apply req.negotiation copy(id = req.id))
        }
        if(allRequests(req.id)) {
          val resp = decide.get apply req.negotiation apply requests.toMap.mapValues(_.get)
          addConfirmEntry(resp)
          log.debug(s"PriorityNegotiationHandlerSpec: process: resp=$resp")
          sendToAll(resp)
        }
        else log.debug(s"requests = $requests")
      case resp: Lang#PriorityRaiseResponse if owner.get(resp.negotiation).currentState() == NegotiationState.NegotiatingPriority =>
        import owner.log
        addConfirmEntry(resp)
        if(allConfirmations(resp.respondingTo))
          onPriorityUpdate.get apply(resp.negotiation, confirmations.toMap.mapValues(_.get))
        else log.debug(s"confirmations = $confirmations")
    }
  )
  lazy val evidence = new DefDS[NegotiationId => Any](_ => ???)

  lazy val onPriorityUpdate = new DefBADS[(NegotiationId, Map[AgentRef, Lang#PriorityRaiseResponse]) => Unit](owner => ???)

  lazy val decide = new DefDS[NegotiationId => Map[AgentRef, Lang#PriorityRaiseRequest] => Lang#PriorityRaiseResponse](_ => ???)

  lazy val start = new DefDS[(NegotiationId) => Lang#PriorityRaiseRequest](
    implicit owner => {
      neg =>
        owner.resendDelayedMessages()
        implicit def sender = owner.ref
        owner.get(neg).currentState update NegotiationState.NegotiatingPriority
        owner.log.debug(s"owner.ref = " + sender)
        val reqId = Message.PriorityRaiseRequestId()
        val msg = Message.PriorityRaiseRequest(reqId, neg, owner.get(neg).currentPriority(), evidence.get.apply(neg))(sender).asInstanceOf[Lang#PriorityRaiseRequest]
        //requests += reqId -> (requestsMap(owner.get(neg)) += owner.ref -> Some(msg))//mutable.Map.empty[AgentRef, Option[Lang#PriorityRaiseRequest]].withDefaultValue(None)
        addReqEntry(msg)
        msg
    }
  )

}
