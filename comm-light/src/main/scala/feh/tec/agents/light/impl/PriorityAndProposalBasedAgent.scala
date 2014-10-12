package feh.tec.agents.light.impl

import akka.actor.{ActorLogging, Props}
import feh.tec.agents.light.Message.{PriorityRaiseRequestId, PriorityRaiseResponse, PriorityRaiseRequest, ProposalId}
import feh.tec.agents.light.NegotiationState.{Starting, Negotiating, Stopped}
import feh.tec.agents.light._
import feh.tec.agents.light.impl.PriorityAndProposalBasedAgent.Default.DefaultPriorityNegotiationHandler
import feh.tec.agents.light.spec.{HiddenMonoDefinition, AgentSpecification, MonoDefinition, ExtendableDefinition}
import feh.util._

import scala.collection.mutable

class PriorityAndProposalBasedAgent[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityProposalBasedAgent[Lang] with DynamicScopeSupport[Lang] with SpeakingSystemSupport[Lang] with ActorLogging
{
  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang]

  /* should be defined by ExtendableDefinition */

  def nothingToPropose(neg: NegotiationId): Unit = ???

  protected def beforeEachMessage(msg: Lang#Msg): Unit = ???

  def setNextProposal(neg: NegotiationId): Unit = ???

  def nextValues(neg: NegotiationId): Option[Map[Var, Any]] = ???

  def onProposal: PartialFunction[Lang#Proposal, Any] = ???

  def onRejection: PartialFunction[Lang#Rejection, Any] = ???

  def onAcceptance: PartialFunction[Lang#Acceptance, Any] = ???

  def updateCurrentProposal(neg: NegotiationId): Unit = ???

  def priorityNegotiationHandler: PriorityNegotiationHandler[Lang] = ???

  def stop(): Unit = ???

  def reset(): Unit = ???

  def start(): Unit = ???

  /* == == == == Utils == == == == */

  object myPriority{
    def isHigherThenOf(msg: Lang#Msg) = comparePriority(msg, _ > _)
    def isLowerThenOf(msg: Lang#Msg) = comparePriority(msg, _ < _)
  }

  /** (this, that) => Boolean */
  def comparePriority(msg: Lang#Msg, f: (Priority, Priority) => Boolean): Boolean =
    f(get(msg.negotiation).currentPriority(), msg.priority)

  def requestPriorityRaise(neg: NegotiationId): Lang#PriorityRaiseRequest = ???



  def process: PartialFunction[Lang#Msg, Any] = {
    case prop: Lang#Proposal   => onProposal lift prop
    case resp: Lang#Acceptance => onAcceptance lift resp
    case resp: Lang#Rejection  => onRejection lift resp
    case priority: Lang#Priority =>
  }

  val name: String = ???
  override val role: NegotiationRole = ???
  implicit val ref: AgentRef = ???
  def negotiations: Set[Negotiation] = ???

}

abstract class PriorityNegotiationHandlerImpl[Lang <: Language.ProposalBased with Language.HasPriority](
                                          get: NegotiationId => AbstractNegotiation,
                                          sendAll: Lang#Msg => Unit
                                          )
  extends PriorityNegotiationHandler[Lang]
{
  protected lazy val requests = mutable.HashMap.empty[PriorityRaiseRequestId, mutable.HashMap[AgentRef, Option[PriorityRaiseRequest[_]]]]
  protected lazy val confirmations = mutable.HashMap.empty[PriorityRaiseRequestId, mutable.HashMap[AgentRef, Option[PriorityRaiseResponse]]]
  protected def clear(id: PriorityRaiseRequestId) = {
    requests -= id
    confirmations -= id
  }
  protected def requestsMap(neg: NegotiationId) = mutable.HashMap(get(neg).scope.toSeq.zipMap(_ => Option.empty[PriorityRaiseRequest[_]]): _*)
  def allRequests(id: PriorityRaiseRequestId): Boolean = requests.get(id).exists(_.forall(_._2.isDefined))

  def process = {
    case req: Lang#PriorityRaiseRequest =>
      requests.getOrElseUpdate(req.id, requestsMap(req.negotiation))(req.sender) = Some(req)
      if(allRequests(req.id)) sendAll(decide(requests(req.id).toMap.mapValues(_.get)).asInstanceOf[Lang#Msg])
  }

  def onPriorityUpdate(f: (Option[Priority]) => Any): Unit = ???

  def decide(requests: Map[AgentRef, PriorityRaiseRequest[_]]): PriorityRaiseResponse = ???

  def start(neg: NegotiationId): Lang#PriorityRaiseRequest = ???
}


object PriorityAndProposalBasedAgent{

  trait Default[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]
    extends AgentSpecification.PriorityAndProposalBased[Ag, Lang]
  {
//    type BuildArgs =

    lazy val nothingToPropose = new DefBADS[NegotiationId => Unit](_ => id => sys.error(s"$id failed: nothing to propose"))

    protected lazy val beforeEachMessage = new DefDS[Lang#Msg => Unit](_ => _ => {})

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
      new Default.DefaultPriorityNegotiationHandler[Ag, Lang](???){} // todo
    )

    lazy val start = new DefBADS[Unit](_.negotiations.foreach(_.currentState update Starting))
    lazy val stop =  new DefBADS[Unit](_.negotiations.foreach(_.currentState update Stopped))
    lazy val reset = new DefBADS[Unit](_ => {})

    def build(args: BuildArgs): Props = ???

  }

  object Default{
//    case class DomainIteratorEvidence()

    class DefaultPriorityNegotiationHandler[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority](
//            get: NegotiationId => Negotiation.HasPriority,
//            sendAll: Lang#Msg => Unit,
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
        mutable.HashMap(owner.get(neg).scope.toSeq.zipMap(_ => Option.empty[Lang#PriorityRaiseRequest]): _*)

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
  }
}