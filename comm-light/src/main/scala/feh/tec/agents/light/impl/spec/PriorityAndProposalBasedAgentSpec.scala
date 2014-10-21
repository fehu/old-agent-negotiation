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

  lazy val nothingToPropose = new DefBADS[NegotiationId => Unit](_ => id => sys.error(s"$id failed: nothing to propose"))

  lazy val beforeEachMessage = new DefDS[Lang#Msg => Unit](_ => _ => {})

  lazy val setNextProposal = new DefBADS[NegotiationId => Unit](
    implicit owner => {
      negId =>
        val neg = owner.get(negId)
        nextValues.get.apply(negId)
          .map{ neg.currentValues.update } getOrElse nothingToPropose.get.apply(negId)
    }
  )

  //    lazy val nextValues: DefBADS[NegotiationId => Option[Map[Var, Any]]] = new DefBADS[NegotiationId => Option[Map[Var, Any]]](???) // todo

  lazy val onProposal = new DefDS[PartialFunction[Lang#Proposal, Any]](Map())
  lazy val onAcceptance = new DefDS[PartialFunction[Lang#Acceptance, Any]](Map())
  lazy val onRejection = new DefDS[PartialFunction[Lang#Rejection, Any]](Map())

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

  lazy val priorityNegotiationHandler = new DefDSH[AgentSpecification.PriorityNegotiationHandler[Ag, Lang]](o =>
    new PriorityNegotiationHandlerSpec[Ag, Lang](???) // todo
  )

  lazy val initialize = new DefBADS[Unit](_.negotiations.foreach(_.ensuring(_.scope().nonEmpty).currentState update Initialized ))
  lazy val start = new DefBADS[Unit](_.negotiations.foreach(_.currentState update Starting))
  lazy val stop =  new DefBADS[Unit](_.negotiations.foreach(_.currentState update Stopped))
  lazy val reset = new DefBADS[Unit](_ => {})

}

class PriorityNegotiationHandlerSpec[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority](
      evidence: NegotiationId => Any
      )
  extends AgentSpecification.PriorityNegotiationHandler[Ag, Lang]
{
  protected lazy val requests = mutable.HashMap.empty[PriorityRaiseRequestId, mutable.HashMap[AgentRef, Option[Lang#PriorityRaiseRequest]]]
  protected lazy val confirmations = mutable.HashMap.empty[PriorityRaiseRequestId, mutable.HashMap[AgentRef, Option[Lang#PriorityRaiseResponse]]]
  protected def clear(id: PriorityRaiseRequestId) = {
    requests -= id
    confirmations -= id
  }
  protected def requestsMap(neg: NegotiationId)(implicit owner: Ag) =
    mutable.HashMap(owner.get(neg).scope().toSeq.zipMap(_ => Option.empty[Lang#PriorityRaiseRequest]): _*)

  def allRequests(id: PriorityRaiseRequestId): Boolean = requests.get(id).exists(_.forall(_._2.isDefined))

  protected def addReqEntry(neg: NegotiationId, req: Lang#PriorityRaiseRequest)(implicit owner: Ag) =
    requests.getOrElseUpdate(req.id, requestsMap(req.negotiation))(req.sender) = Some(req)

  lazy val process = new DefDS[PartialFunction[Lang#Priority, Any]](
    implicit owner => {
      case req: Lang#PriorityRaiseRequest =>
        addReqEntry(req.negotiation, req)
        val x = decide.get apply requests(req.id).toMap.mapValues(_.get)
        if(allRequests(req.id)) owner.sendToAll(x)
    }
  )

  lazy val onPriorityUpdate = new DefBADS[(NegotiationId, Option[Priority]) => Unit](owner => (neg, prOpt) => {}) //todo: does nothing

  lazy val decide: DefDS[(Map[AgentRef, Lang#PriorityRaiseRequest]) => Lang#PriorityRaiseResponse] = ???

  lazy val start = new DefDS[(NegotiationId) => Lang#PriorityRaiseRequest](
    implicit owner => {
      neg =>
        implicit def sender = owner.ref
        val msg = Message.PriorityRaiseRequest(Message.PriorityRaiseRequestId.rand, neg, owner.get(neg).currentPriority(), evidence(neg))
        addReqEntry(neg, msg.asInstanceOf[Lang#PriorityRaiseRequest])
        msg.asInstanceOf[Lang#PriorityRaiseRequest]
    }
  )

}
