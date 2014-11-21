package feh.tec.agents.lite.impl.spec

import feh.tec.agents.lite.Message.{PriorityRaiseRequestId, ProposalId}
import feh.tec.agents.lite.NegotiationState.{Initialized, Starting, Stopped}
import feh.tec.agents.lite._
import feh.tec.agents.lite.impl.PriorityAndProposalBasedAgent
import feh.tec.agents.lite.spec.AgentSpecification
import feh.util._
import scala.collection.mutable

trait PriorityAndProposalBasedAgentSpec[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]
  extends AgentSpecification.PriorityAndProposalBased[Ag, Lang]   {
  //    type BuildArgs =

  lazy val nothingToPropose = new DefBADS[NegotiationId => Unit](_ => id => sys.error(s"$id: nothing to propose"))

  lazy val beforeEachMessage = new DefDS[Lang#Msg => Unit](_ => _ => {})

  lazy val setNextProposal = new DefBADS[NegotiationId => Lang#Proposal](
    implicit owner => {
      negId =>
        val neg = owner.get(negId)
        val nextVals = owner.nextValues(negId)
        nextVals.map{ neg.currentValues.update } getOrElse owner.nothingToPropose(negId) //nothingToPropose.get.apply(negId)
        owner.updateCurrentProposal(negId)
        neg.currentProposal()
//        neg.currentProposal update Message.Proposal(Message.ProposalId.rand, negId, neg.currentPriority(), nextVals.get)(owner.ref)
    }
  )

  //    lazy val nextValues: DefBADS[NegotiationId => Option[Map[Var, Any]]] = new DefBADS[NegotiationId => Option[Map[Var, Any]]](???) // todo

  private def noDefErr[Msg](in: String): Ag => PartialFunction[Msg, Any] = (ag: Ag) => {case _: Msg => sys.error(s"no `$in` behavior defined") }

  lazy val onProposal = new DefDS[PartialFunction[Lang#Proposal, Any]](noDefErr("onProposal"))
  lazy val onAcceptance = new DefDS[PartialFunction[Lang#Acceptance, Any]](noDefErr("onAcceptance"))
  lazy val onRejection = new DefDS[PartialFunction[Lang#Rejection, Any]](noDefErr("onRejection"))

  lazy val moreProcess = new DefDS[PartialFunction[Lang#Msg, Any]](_ => Map())
  lazy val processUserMessage = new DefDS[PartialFunction[UserMessage, Any]](_ => Map())

  protected def proposal(negId: NegotiationId)(implicit owner: Ag) = {
      val neg = owner.get(negId)
      Message.Proposal(ProposalId.rand, negId, neg.currentPriority(), neg.currentValues())(owner.ref)

  }

  lazy val updateCurrentProposal = new DefBADS[NegotiationId => Unit](
    implicit owner => {
      negId =>
        owner.get(negId).currentProposal update proposal(negId)
    })

  lazy val initialize = new DefBADS[Unit](_.negotiations.foreach(_.ensuring(_.scope().nonEmpty).currentState update Initialized ))
  lazy val start = new DefBADS[Unit](
    ag => {
      ag.negotiations.foreach(_.currentState update Starting)
      ag.resendDelayedMessages()
    }

  )
  lazy val stop =  new DefBADS[Unit](_.negotiations.foreach(_.currentState update Stopped))
  lazy val reset = new DefBADS[Unit](_ => {})

}

object PriorityAndProposalBasedAgentSpec{
  trait NegotiatingPriority[Ag <: PriorityAndProposalBasedAgent.NegotiatesPriority[Lang], Lang <: Language.ProposalBased with Language.HasPriorityNegotiation]
    extends PriorityAndProposalBasedAgentSpec[Ag, Lang] with AgentSpecification.NegotiatesPriority[Ag, Lang]
  {
    lazy val requestPriorityRaise = new DefBADS[NegotiationId => Lang#PriorityRaiseRequest](
      implicit owner => id => priorityNegotiationHandler.get.start.get apply id
    )

    protected var priorityNegotiationHandlerSetup = List.empty[AgentSpecification.PriorityNegotiationHandler[Ag, Lang] => Unit]
    def priorityNegotiationHandler[R](f: AgentSpecification.PriorityNegotiationHandler[Ag, Lang] => Unit) = priorityNegotiationHandlerSetup ::= f

    protected[lite] lazy val priorityNegotiationHandler = new DefDSH[AgentSpecification.PriorityNegotiationHandler[Ag, Lang]](
      implicit owner =>
        new PriorityNegotiationHandlerSpec[Ag, Lang] $$ (spec => priorityNegotiationHandlerSetup.reverse.foreach(_(spec)))
    )
  }
}
/*

 */

class PriorityNegotiationHandlerSpec[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriorityNegotiation]
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
    // if there are already requests guarded, select the created first
    if(raiseRequestId.isEmpty || (req.id.initializedAt before raiseRequestId.get.initializedAt)){
      clear()
      raiseRequestId = Some(req.id)
      requests ++= emptyMap(req)
    }
    else requests(req.sender) = Some(req)
  }

  protected def addConfirmEntry(resp: Lang#PriorityRaiseResponse)(implicit owner: Ag) ={
    val sndr = resp.sender
    if(confirmations.isEmpty) confirmations ++= emptyMap(resp)
    else confirmations(sndr) = Some(resp)
  }

  lazy val process = new DefDS[PartialFunction[Lang#Priority, Any]](
    implicit owner => {
      case req: Lang#PriorityRaiseRequest =>
        import owner.{log, ref, sendToAll}
        owner.get(req.negotiation).currentState update NegotiationState.NegotiatingPriority
        addReqEntry(req)
        if(requests(ref).isEmpty) sendToAll(start.get apply req.negotiation copy(id = req.id))
        if(allRequests(req.id)) {
          val resp = decide.get apply req.negotiation apply requests.toMap.mapValues(_.get)
          addConfirmEntry(resp)
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
        implicit def sender = owner.ref
        owner.get(neg).currentState update NegotiationState.NegotiatingPriority
        val reqId = Message.PriorityRaiseRequestId()
        val msg = Message.PriorityRaiseRequest(reqId, neg, owner.get(neg).currentPriority(), evidence.get.apply(neg))(sender).asInstanceOf[Lang#PriorityRaiseRequest]
        addReqEntry(msg)
        msg
    }
  )

}
