package feh.tec.agents.light

import feh.tec.agents.light.impl.agent.create
import feh.util._
import feh.tec.agents.light.spec.RequiresDistinctPriority

import scala.collection.mutable

object QueenSpec{
  def apply() = new QueenSpec
}

class QueenSpec extends create.PPI.AllVarsSpec with RequiresDistinctPriority{

//  val priorities = mutable.HashMap.empty[AgentRef, Priority]
  private val proposalAcceptance = mutable.HashMap.empty[NegotiationId, mutable.HashMap[AgentRef, Option[Boolean]]]

  def setProposalAcceptance(neg: NegotiationId, ref: AgentRef)(v: Boolean)(implicit ag: create.PPI.Ag) = proposalAcceptance
    .getOrElseUpdate(neg, mutable.HashMap(ag.get(neg).scope().zipMap(_ => Option.empty[Boolean]).toSeq: _*))(ref) = Option(v)
  def clearProposalAcceptance(neg: NegotiationId) = proposalAcceptance -= neg
  def allAccepted(neg: NegotiationId) = proposalAcceptance.exists(_._2.forall(_._2.exists(identity)))

  def whenProposalAccepted(neg: NegotiationId)(implicit ag: create.PPI.Ag) = if(allAccepted(neg)){
    ag.log.info("all accepted, setting state to **Waiting** + " + proposalAcceptance(neg))
    ag.get(neg).currentState update NegotiationState.Waiting
  }
  
  initialize after {
    ag => _ =>
//      ag.negotiations.foreach(_.currentPriority update new Priority(1))
      ag.log.info("initialized")
  }
  start andThen {
    ag =>
      overridden =>
        import ag._
        overridden(ag)
        negotiations.foreach {
          neg =>
            if(neg.currentIterator.raw.isEmpty) neg.currentIterator update ag.newIterator(neg.id)
            log.info("Iterating the domain with " + neg.currentIterator())
            ag.setNextProposal(neg.id)
            //            ag.nextValues()
            //            val proposal = neg.currentProposal.getOrElse {
            //              Message.Proposal(Message.ProposalId.rand, neg.id, neg.currentPriority(), neg.currentValues())
            //            }
            neg.currentState update NegotiationState.Negotiating
            log.info(s"negotiation ${neg.id} started, scope = ${neg.scope()}")
            sendToAll(neg.currentProposal())
        }
  }

  def respondToProposal(msg: Message.Proposal)(implicit ag: create.PPI.Ag) = {
    import ag._
    msg match{
      case Message.Proposal(propId, negId, _, values) =>
        val response =
          if(msg.satisfiesConstraints) Message.Accepted(negId, propId, get(negId).currentPriority(), get(negId).currentValues())
          else Message.Rejected(negId, propId, get(negId).currentPriority(), get(negId).currentValues())
//        priorities += msg.sender -> msg.priority
        respond(response)
    }
  }

  onProposal <:= {
    implicit ag =>{
      case msg if ag.myPriority isHigherThenOf msg => respondToProposal(msg)
      case msg if ag.myPriority isLowerThenOf  msg => respondToProposal(msg)
    }
  }

  onRejection <:= {
    implicit ag => {
      case msg if msg.respondingTo != ag.get(msg.negotiation).currentProposal().id => ag.log.debug(s"!!!!Old message ignored: msg.respondingTo = ${msg.respondingTo}, mine=${ag.get(msg.negotiation).currentProposal().id}") //ignore
      case msg if ag.myPriority isHigherThenOf msg =>
        //mark as accepted
        setProposalAcceptance(msg.negotiation, msg.sender)(true)
        whenProposalAccepted(msg.negotiation)
        ag.log.debug("rejected: myPriority isHigherThenOf " + msg)
      case msg if ag.myPriority isLowerThenOf  msg =>
        setProposalAcceptance(msg.negotiation, msg.sender)(false)
        whenProposalAccepted(msg.negotiation)
        ag.log.debug("!!!!!!!!!!!!!! rejected")
        val prop = ag.setNextProposal(msg.negotiation)
        ag.log.debug("rejected: spam new proposal " + prop)
        ag.sendToAll(prop)
    }
  }

  setNextProposal andThen {
    ag =>
      overridden =>
        negId =>
          clearProposalAcceptance(negId)
          overridden(ag)(negId)
  }

//  protected def onAcceptanceAndRejectionIfMyPriorityIsHigher(resp: Message.ProposalResponse) = {
//    proposalAcceptance.getOrElseUpdate(resp.negotiation, mutable.HashMap.empty) += resp.sender -> resp
//    priorities += resp.sender -> resp.priority
//  }

  onAcceptance <:= {
    implicit ag => {
      case msg =>
        setProposalAcceptance(msg.negotiation, msg.sender)(true)
        whenProposalAccepted(msg.negotiation)
    }
  }

}


@deprecated("the priority negotiation scheme must be reconsidered")
trait PriorityNegotiationHandlerSetup {
  self: create.PPI.AllVarsSpec =>

  val lowPercentageLimit = .5

  val alreadyTriedCount = mutable.Map.empty[NegotiationId, Int].withDefaultValue(0)

  case class IteratorEvidence(negId: NegotiationId, alreadyTried: Int)

  def ratioOf[T](in: Iterable[T])(count: T => Boolean) = in.count(count).toDouble / in.size

  nextValues andThen{
    ag =>
      overridden =>
        negId =>
          alreadyTriedCount(negId) += 1
          overridden(ag)(negId)
  }

  priorityNegotiationHandler{
    h =>
      h.onPriorityUpdate <:= {
        implicit ag => {
          case (negId, confirmations) => // the agent, that chose to raise the priority must keep their relative order (if it is not the same)
            val myDecision = confirmations(ag.ref)
            if(myDecision.isInstanceOf[Message.Raise]) {
              val countRaisingWithInitialPriorityLowerThenMine = confirmations.withFilter(_._1 != ag.ref).map(_._2.priority).count(_ < myDecision.priority)
              ag.log.debug(s"countRaisingWithInitialPriorityLowerThenMine = $countRaisingWithInitialPriorityLowerThenMine")
              val newPriority = myDecision.priority.raise(countRaisingWithInitialPriorityLowerThenMine + 1)
              ag.log.debug(s"what to sen new priority $newPriority")
              ag.get(negId).currentPriority update newPriority
              ag.updateCurrentProposal(negId)
              ag.sendToAll(proposal(negId))
            }
            //else do nothing
        }
      }
      h.decide <:= {
        ag =>
          negId =>
            requests =>
              val myReq = requests(ag.ref)
              val reqId = myReq.id
              val my = myReq.evidence.asInstanceOf[IteratorEvidence].alreadyTried
              val myPriority = ag.get(negId).currentPriority()
              val (myWeight, othersWeights) =
                if(requests.forall(_._2.evidence.asInstanceOf[IteratorEvidence].alreadyTried == my))
                  ag.ref.hashCode() -> requests.withFilter(_._1 != ag.ref).map(_._1.hashCode())
                else my -> requests.withFilter(_._1 != ag.ref).map(_._2.evidence.asInstanceOf[IteratorEvidence].alreadyTried)

              import ag._
              val resp = if(ratioOf(othersWeights)(_ > myWeight) > lowPercentageLimit) Message.Keep(negId, reqId, myPriority)
                else Message.Raise(negId, reqId, myPriority)

              resp.asInstanceOf[create.PPI.Lang#PriorityRaiseResponse]
        }
      h.evidence <:={ _ => negId => IteratorEvidence(negId, alreadyTriedCount(negId)) }
  }

}