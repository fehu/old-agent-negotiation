package feh.tec.agents.light

/*

@deprecated("the priority negotiation scheme must be reconsidered")
trait PriorityNegotiationHandlerSetup extends PriorityAndProposalBasedAgentSpec.NegotiatingPriority[]{
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

}*/
