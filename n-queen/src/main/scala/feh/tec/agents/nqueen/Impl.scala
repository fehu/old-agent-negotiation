package feh.tec.agents.nqueen

import feh.tec.agents.View.Estimation
import feh.tec.agents.Views._
import feh.tec.agents._

object Impl {
  val Size = 4
  
  
  object X extends Var("x", _.isInstanceOf[Int]) with Domain.Range{ def domain = 1 to Size }
  object Y extends Var("y", _.isInstanceOf[Int]) with Domain.Range{ def domain = 1 to Size }

  implicit object XYIterator extends DomainIterator.Range()


  implicit class VarWrapper[V <: Var, T](v: V){
    def it(implicit it: DomainIterator[V#Domain, T]) = v -> it(v.domain)
    def view(expectingResponseTo: Message.Id => Boolean,
             buildOpinion: VarView with Views.DefaultSchema => PartialFunction[Message, Estimation.Opinion]) =
      new VarView(v) with Views.DefaultSchema with View.Default
      {
        def expectingResponse = expectingResponseTo
        def opinion: PartialFunction[Message, Estimation.Opinion] = buildOpinion(this)
      }
  }

/*
  trait Queen extends BacktrackAgent.Default with StaticScope with DomainIterators with StaticScopeAndIteratorInit {
    def iteratorsInit = Map(X.it, Y.it)

    val XView = X.view(expectingResponse, Views.VarView.DefaultSchema)
    val YView = Y.view(expectingResponse, Views.VarView.DefaultSchema)
    val views = Set(XView, YView)

    def buildProposal(values: Map[Var, Any], sender: AgentRef, priority: Priority) = Proposal(values)(sender, priority)

    def acceptable_?(issues: Map[Var, Any]) = ???

    def changeValueAt: InUnitInterval

    // ignore the message
    protected def answerReceived(msg: Response) = {
      val xDistr = utils.opinionDistribution(in = Set(XView), select = Estimation.Constraint.select)
      val yDistr = utils.opinionDistribution(in = Set(YView), select = Estimation.Constraint.select)

      val toChange = Set(
        X inCase (_ => xDistr(Violated)._2 >= changeValueAt),
        Y inCase (_ => yDistr(Violated)._2 >= changeValueAt)
      ).flatten
      setNextProposal()
    }

    protected def fallbackRequested(msg: Fallback) = ???
  }
*/
}