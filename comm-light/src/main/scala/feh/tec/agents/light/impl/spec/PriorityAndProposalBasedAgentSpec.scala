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
        neg.currentProposal update Message.Proposal(Message.ProposalId.rand, negId, neg.currentPriority(), nextVals.get)(owner.ref)
    }
  )

  //    lazy val nextValues: DefBADS[NegotiationId => Option[Map[Var, Any]]] = new DefBADS[NegotiationId => Option[Map[Var, Any]]](???) // todo

  private def noDefErr[Msg](in: String): Ag => PartialFunction[Msg, Any] = (ag: Ag) => {case _: Msg => sys.error(s"no `$in` behavior defined") }

  lazy val onProposal = new DefDS[PartialFunction[Lang#Proposal, Any]](noDefErr("onProposal"))
  lazy val onAcceptance = new DefDS[PartialFunction[Lang#Acceptance, Any]](noDefErr("onAcceptance"))
  lazy val onRejection = new DefDS[PartialFunction[Lang#Rejection, Any]](noDefErr("onRejection"))

  def proposal(negId: NegotiationId)(implicit owner: Ag) = owner.get(negId) |> {
    neg =>
      Message.Proposal(ProposalId.rand, negId, neg.currentPriority(), neg.currentValues())(owner.ref)

  }

  lazy val updateCurrentProposal = new DefBADS[NegotiationId => Unit](
    implicit owner =>
      negId => owner.get(negId).currentProposal update proposal(negId)

  )

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
  protected lazy val requests = mutable.HashMap.empty[PriorityRaiseRequestId, mutable.Map[AgentRef, Option[Lang#PriorityRaiseRequest]]]
  protected lazy val confirmations = mutable.HashMap.empty[PriorityRaiseRequestId, mutable.Map[AgentRef, Option[Lang#PriorityRaiseResponse]]]
  protected def clear(id: PriorityRaiseRequestId) = {
    requests -= id
    confirmations -= id
  }
  protected def emptyMap[T](neg: Ag#Negotiation)(implicit owner: Ag) =
    mutable.HashMap((neg.scope() + owner.ref).toSeq.zipMap(_ => Option.empty[T]): _*)

  def allRequests(id: PriorityRaiseRequestId): Boolean = requests.get(id).exists(_.forall(_._2.isDefined))
  def allConfirmations(id: PriorityRaiseRequestId): Boolean = confirmations.get(id).exists(_.forall(_._2.isDefined))

  protected def addReqEntry(req: Lang#PriorityRaiseRequest)(implicit owner: Ag) ={
    val snd = req.sender
    owner.log.debug(s"adding priority raise request by $snd")
    requests.getOrElseUpdate(req.id, emptyMap(owner.get(req.negotiation))) += snd -> Some(req)
  }

//  protected def confirmationsMap(neg: Ag#Negotiation)(implicit owner: Ag) =
//    mutable.HashMap((neg.scope() + owner.ref).toSeq.zipMap(_ => Option.empty[Lang#PriorityRaiseResponse]): _*)
  
  protected def addConfirmEntry(resp: Lang#PriorityRaiseResponse)(implicit owner: Ag) ={
    val snd = resp.sender
    owner.log.debug(s"adding priority raise request by $snd")
    confirmations.getOrElseUpdate(resp.respondingTo, emptyMap(owner.get(resp.negotiation))) += snd -> Some(resp)
  }

  lazy val process = new DefDS[PartialFunction[Lang#Priority, Any]](
    implicit owner => {
      case req: Lang#PriorityRaiseRequest =>
        import owner.{log, ref, sendToAll}
        addReqEntry(req)
        log.debug(s"PriorityNegotiationHandlerSpec: process: $req by ${req.sender}")
        log.debug(s"requests(req.id)(ref)=${requests(req.id)(ref)} in ${requests(req.id)} ")
        if(requests(req.id)(ref).isEmpty) {
          log.debug("sending my priority raise request")
          sendToAll(start.get apply req.negotiation copy(id = req.id))
        }
        if(allRequests(req.id)) {
          val resp = decide.get apply req.negotiation apply requests(req.id).toMap.mapValues(_.get)
          addConfirmEntry(resp)
          log.debug(s"PriorityNegotiationHandlerSpec: process: resp=$resp")
          sendToAll(resp)
        }
        else log.debug(s"requests = ${requests.get(req.id)}")
      case resp: Lang#PriorityRaiseResponse =>
        import owner.log
        addConfirmEntry(resp)
        if(allConfirmations(resp.respondingTo))
          onPriorityUpdate.get apply(resp.negotiation, confirmations(resp.respondingTo).toMap.mapValues(_.get))
        else log.debug(s"confirmations = ${confirmations(resp.respondingTo)}")
    }
  )
  lazy val evidence = new DefDS[NegotiationId => Any](_ => ???)

  lazy val onPriorityUpdate = new DefBADS[(NegotiationId, Map[AgentRef, Lang#PriorityRaiseResponse]) => Unit](owner => ???)

  lazy val decide = new DefDS[NegotiationId => Map[AgentRef, Lang#PriorityRaiseRequest] => Lang#PriorityRaiseResponse](_ => ???)

  lazy val start = new DefDS[(NegotiationId) => Lang#PriorityRaiseRequest](
    implicit owner => {
      neg =>
        implicit def sender = owner.ref
        owner.log.debug(s"owner.ref = " + sender)
        val reqId = Message.PriorityRaiseRequestId.rand
        val msg = Message.PriorityRaiseRequest(reqId, neg, owner.get(neg).currentPriority(), evidence.get.apply(neg))(sender).asInstanceOf[Lang#PriorityRaiseRequest]
        //requests += reqId -> (requestsMap(owner.get(neg)) += owner.ref -> Some(msg))//mutable.Map.empty[AgentRef, Option[Lang#PriorityRaiseRequest]].withDefaultValue(None)
        addReqEntry(msg)
        msg
    }
  )

}
