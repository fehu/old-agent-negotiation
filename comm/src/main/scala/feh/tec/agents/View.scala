package feh.tec.agents

import feh.tec.agents.Message.{Rejected, Accepted}
import feh.tec.agents.View.Estimation.Opinion
import feh.util.InUnitInterval

import scala.collection.mutable

object View{
  type Default = Impl.HashMapView

  object Estimation{
    trait Opinion

    case object Unknown extends Opinion

    object Constraint{
      def select: PartialFunction[Opinion, Opinion] = {
        case op: Respected => Respected
        case op: Violated => Violated
        case Unknown => Unknown
      }

      trait Respected extends Opinion
      object Respected extends Respected

      trait Violated extends Opinion
      object Violated extends Violated
    }
  }

  // todo: history based on value changes
  object Impl{
    trait HashMapView extends View{
      protected val _opinions = mutable.HashMap.empty[AgentRef, Estimation.Opinion]

      def opinions = _opinions.toMap

      def newOpinion(of: AgentRef, e: Estimation.Opinion) = _opinions += of -> e

      def forget(about: AgentRef) = if(_opinions.contains(about)) _opinions(about) = Estimation.Unknown
    }
  }

  trait Helper{
    self: View =>

    def collectEstimations(f: PartialFunction[Estimation.Opinion, Map[AgentRef, Estimation.Opinion]]) =
      self.opinions.filter( f isDefinedAt _._2 )
  }

}

/** A view over an aspect of relation
  *
  */
trait View{
  import View._

  def aspect: String
  def opinions: Map[AgentRef, Estimation.Opinion]

  /** Updates view's opinion on the interlocutor
    *  Opinion update on [[Estimation.Unknown]] happens only if no opinion yet exists for the interlocutor
    *  To reset view's opinion to Unknown use `forget` method
    */
  protected def newOpinion(of: AgentRef, e: Estimation.Opinion)

  /** reset view's opinion about the interlocutor to [[Estimation.Unknown]]  */
  def forget(about: AgentRef)

  protected def opinion: PartialFunction[Message, Estimation.Opinion]

  /** Ignores unknown messages */
  def process(msg: Message) = opinion.lift(msg) map (newOpinion(msg.sender, _))

}

/*
object Views{
  import View._
  import Estimation.Constraint

  trait DefaultSchema{
    self: View =>

    def expectingResponse: Message.Id => Boolean
  }

  object VarView{
    def DefaultSchema(view: VarView with DefaultSchema): PartialFunction[Message, Estimation.Opinion] = {
      case Accepted(offerId) if view.expectingResponse(offerId) => new Constraint.Respected {}
      case Rejected(offerId) if view.expectingResponse(offerId) => new Constraint.Violated {}
      case msg: Message => Estimation.Unknown
    }

  }

  abstract class VarView(val v: Var) extends View{
    def aspect = s"Var($v) constraint violations"
  }

  trait PriorityView extends View{

  }

}
*/

/** Agent's views of its position in the negotiation TODO
 */
trait Views {
  self: NegotiatingAgent[_] =>

  def views: Set[View]

  protected def processMsgViews(msg: Message) = views.par foreach (_.process(msg))

  object utils{
//    trait OpinionDistribution{
//      trait Entry{
//        def opinion: Opinion
//        def about: AgentRef
//      }
//    }

    def opinionDistribution[T](select: PartialFunction[Opinion, T], in: Set[View] = views): Map[T, (Set[AgentRef], InUnitInterval)] = {
      val results = views.toSeq.flatMap(_.opinions.toSeq.collect{
        case (ag, op) if select isDefinedAt op => select(op) -> ag
      })
      val count = results.groupBy(_._1).map{
        case (t, mp) => t -> (mp.map(_._2).toSet -> mp.length)
      }
      val n = count.map(_._2._2).sum
      count.mapValues{ case (ags, i) => ags -> (i.toDouble / n) }
    }

  }

}
