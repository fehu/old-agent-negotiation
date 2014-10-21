package feh.tec.agents.light

import feh.tec.agents.light.NegotiationState.Stopped

import scala.collection.mutable

trait AbstractNegotiation {
  val id: NegotiationId
  val issues: Set[Var]

  def scope: IdentStateVar[Set[AgentRef]]

  protected abstract class StateVar[Repr, Get, Upd](newRepr: Repr, get: Repr => Get){
    private var repr = newRepr

    def raw = repr
    protected def upd: (Upd, Repr) => Repr

    def apply(): Get = get(repr)
    def update(u: Upd) = repr = {
      reset()
      upd(u, repr)
    }
    def update(f: Get => Upd) = repr = {
      reset()
      upd(f(apply()), repr)
    }

    def reset() = repr = newRepr
  }

  protected class IdentStateVar[T](newRepr: T,
                                   get: T => T = identity[T] _,
                                   _upd: (T,  T) => T = (t: T, _: T) => t) extends StateVar[T, T, T](newRepr, get){
    protected def upd = _upd
  }

  protected class OptionStateVar[T](newRepr: Option[T] = None,
                                    get: Option[T] => T = (_: Option[T]).get,
                                    _upd: (T,  Option[T]) => Option[T] = (t: T, _: Option[T]) => Option(t)) extends StateVar[Option[T], T, T](newRepr, get)
  {
    protected def upd = _upd
    def opt = raw
    def getOrElse(t: => T) = raw getOrElse t
    def map[R](f: T => R) = raw map f
  }

  lazy val currentValues = new StateVar[mutable.HashMap[Var, Any], Map[Var, Any], Map[Var, Any]](
    mutable.HashMap.empty[Var, Any], _.toMap
  ){
    protected def upd = (values, map) =>
      if(currentValuesUpdateAllowed(values)) {
        reset()
        map ++= values
      }
      else sys.error("current values update not allowed")
  }

  lazy val currentState = new IdentStateVar[NegotiationState](Stopped)

  protected def currentValuesUpdateAllowed(values: Map[Var, Any]) = true
}

trait NegotiationState

object NegotiationState {
  case object Created               extends NegotiationState
  case object Initializing          extends NegotiationState
  case object Initialized           extends NegotiationState
  case object Starting              extends NegotiationState
  case object Negotiating           extends NegotiationState
  case object NegotiatingPriority   extends NegotiationState
  case object Waiting               extends NegotiationState
  case object Stopped               extends NegotiationState
}

case class NegotiationId(name: String)

class Priority(val get: Int) extends AnyVal{
  def raise(amount: Int = 1) = new Priority(get + amount)
}
object Priority{
  implicit def priorityToIntWrapper(p: Priority) = p.get
}

object Negotiation{

  trait HasProposal[Lang <: Language.ProposalBased] extends AbstractNegotiation{
    lazy val currentProposal = new OptionStateVar[Lang#Proposal]()
  }

  trait HasPriority extends AbstractNegotiation{
    lazy val currentPriority = new OptionStateVar[Priority]()
  }

  trait DynamicScope extends AbstractNegotiation{
    val scope = new IdentStateVar[Set[AgentRef]](Set())

    def scopeUpdated()
  }

  trait HasIterator extends AbstractNegotiation{
    lazy val currentIterator = new OptionStateVar[ProposalEngine.DomainIterator]()
  }
}