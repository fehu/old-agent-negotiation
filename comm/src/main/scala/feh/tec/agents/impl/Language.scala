package feh.tec.agents.impl

import feh.tec.agents._
import feh.tec.agents.impl.Language.Buildable
import feh.tec.agents.impl.Language.dsl.{Rejection, Acceptation, ProposalSubj}
import feh.util._

object Language {

  object dsl{


    object I extends ProposalTarget{              //(implicit val me: AgentRef)
      def propose(whom: ProposalTarget) = new ChooseSubj(whom)
      def request(whom: ProposalTarget) = new ChooseSubj(whom)
      
      def reject = Rejection()
      def accept = Acceptation()
    }

    trait ProposalTarget
    object You extends ProposalTarget
    
    trait RequestOperation
    case object fallback extends RequestOperation
    
    case class ChoosePair[V <: Var] (issue: V, value: V#Tpe) {
      def pair: (Var, Any) = (issue, value)
    }

    implicit class ChoosePairWrapper[V <: Var](vr: V){
      def ->(vl: V#Tpe)= ChoosePair(vr, vl)
    }

    protected class ChooseSubj(target: ProposalTarget){
      def set[T](value: T): ChooseVar[T] = new ChooseVar(value, target)
      def toSet[T](value: T): ChooseVar[T] = set(value)

      def set(pairs: ChoosePair[_]*): ProposalsSubj = ProposalsSubj(pairs.map(_.pair).toMap, target)
      def toSet(pairs: ChoosePair[_]*): ProposalsSubj = set(pairs: _*)
      
      def `do`(op: RequestOperation) = Request(target, op)
    } 

    protected class ChooseVar[T](value: T, target: ProposalTarget){
      def `for`[V <: Var { type Tpe = T }](issue: V) = new ProposalSubj(issue, value, target)
    }
    
    case class ProposalSubj[T]  protected[dsl] (issue: Var, value: T, target: ProposalTarget) extends Buildable
    case class ProposalsSubj    protected[dsl] (issues: Map[Var, Any], target: ProposalTarget) extends Buildable
    case class Rejection        protected[dsl]() extends Buildable
    case class Acceptation      protected[dsl]() extends Buildable
    case class Request          protected[dsl](target: ProposalTarget, op: RequestOperation) extends Buildable
  }
  
  trait Buildable
  
  trait Builder[Lang <: Language]{
    def buildMessage(negotiation: NegotiationId, b: Buildable): Lang#Msg
  }

}

object DefaultNegotiatingLanguage {
  import Language._

  trait Builder extends Language.Builder[DefaultNegotiatingLanguage]{
    self: NegotiatingAgent =>

    val lang = DefaultNegotiatingLanguage.Static

    def buildMessage(negId: NegotiationId, b: Buildable) = {
      implicit def priority = priorityOf(negId)

      b match{
        case ProposalSubj(issue, v, dsl.I)      => Message.Proposal(negId, Map(issue -> v))
        case ProposalSubj(issue, v, dsl.You)    => Message.Demand  (negId, Map(issue -> v))
        case Acceptation()                      => Message.Accepted(negId, currentMsg.id)
        case Rejection()                        => Message.Rejected(negId, currentMsg.id)
        case dsl.Request(dsl.You, dsl.fallback) => Message.Fallback(negId)
      }}

    private def priorityOf(neg: NegotiationId) = 
      negotiations.find(_.id == neg).map(_.currentPriority) getOrThrow UnknownNegotiation(neg)
  }

  object Static extends DefaultNegotiatingLanguage

  implicit object ExtractIssues extends IssuesExtractor[DefaultNegotiatingLanguage]{
    def extract = {
      case Message.Proposal(_, issues) => issues
    }
  }
}

class DefaultNegotiatingLanguage extends BacktrackLanguage with CounterProposalLanguage /*with feh.tec.agents.Language.Priority*/{
  type Msg = Message

  type Proposal = Message.Proposal
  type Accepted = Message.Accepted
  type Rejected = Message.Rejected

//  type CounterProposal = this.type

  type Fallback = Message.Fallback

  def isMessage(any: Any)         = any.isInstanceOf[Msg]
  def isProposal(msg: Any)        = msg.isInstanceOf[Proposal]
  def isAcceptance(msg: Any)      = msg.isInstanceOf[Accepted]
  def isRejection(msg: Any)       = msg.isInstanceOf[Rejected]
  def isCounterProposal(msg: Any) = msg.isInstanceOf[CounterProposal]
  def isFallback(msg: Any)        = msg.isInstanceOf[Fallback]

}
