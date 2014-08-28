package feh.tec.agents.nqueen

import feh.tec.agents.View.Estimation
import feh.tec.agents._
import feh.tec.agents.impl.{DynamicScopeNegotiation, DynamicScopeSupport, DefaultNegotiatingLanguage, PriorityBasedBacktrackAgent}


object Impl {
  val Size = 4
  
  
  object X extends Var("x", _.isInstanceOf[Int]) with Domain.Range{ def domain = 1 to Size }
  object Y extends Var("y", _.isInstanceOf[Int]) with Domain.Range{ def domain = 1 to Size }

  implicit object XYIterator extends DomainIterator.Range()


  object LangTest {
    import feh.tec.agents.impl.Language.dsl._

    I propose You set 5 `for` X

    I propose I set 1 `for` Y

    I.accept

    I request You `do` fallback
  }

  implicit class VarWrapper[V <: Var, T](v: V){
    def it(implicit it: DomainIterator[V#Domain, T]) = v -> it(v.domain)
//    def view(expectingResponseTo: Message.Id => Boolean,
//             buildOpinion: VarView with Views.DefaultSchema => PartialFunction[Message, Estimation.Opinion]) =
//      new VarView(v) with Views.DefaultSchema with View.Default
//      {
//        def expectingResponse = expectingResponseTo
//        def opinion: PartialFunction[Message, Estimation.Opinion] = buildOpinion(this)
//      }
  }


  object Queen{
    case class Init(priority: Priority, vals: Map[Var, Any])
    
    def negotiation(init: Init) = new DynamicScopeNegotiation(NegotiationId("N-Queen"), init.priority, init.vals)

    def role = new Role{ val name = "Queen" }
  }

  class Queen(val id: impl.Agent.Id, init: Queen.Init) extends PriorityBasedBacktrackAgent[DefaultNegotiatingLanguage]
    with DefaultNegotiatingLanguage.Builder
  {
    val role = Queen.role
    val vars: Set[Var] = Set(X, Y)
    val negotiations = Set(Queen.negotiation(init))

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