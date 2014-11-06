package feh.tec.agents.light

import feh.tec.agents.light.impl.agent.create

import scala.collection.mutable

object QueenSpec extends create.PPI.AllVarsSpec with PriorityNegotiationHandlerSetup{

  val priorities = mutable.HashMap.empty[AgentRef, Priority]
  val proposalAcceptance = mutable.HashMap.empty[NegotiationId, mutable.HashMap[AgentRef, Message.ProposalResponse]]

  initialize after {
    ag => _ =>
      ag.negotiations.foreach(_.currentPriority update new Priority(1))
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
        priorities += msg.sender -> msg.priority
        respond(response)
    }
  }

  onProposal <:= {
    implicit ag =>{
      case msg if ag.myPriority isHigherThenOf msg => respondToProposal(msg)
      case msg if ag.myPriority isLowerThenOf  msg => respondToProposal(msg)
      case samePriority => ag.requestPriorityRaise(samePriority.negotiation)
    }
  }

  protected def onAcceptanceAndRejectionIfMyPriorityIsHigher(resp: Message.ProposalResponse) = {
    proposalAcceptance.getOrElseUpdate(resp.negotiation, mutable.HashMap.empty) += resp.sender -> resp
    priorities += resp.sender -> resp.priority
  }

  onAcceptance <:= {
    implicit ag => {
      case msg if ag.myPriority isHigherThenOf msg =>
    }
  }

}


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