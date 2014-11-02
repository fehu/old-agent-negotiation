package feh.tec.agents.light

import feh.tec.agents.light.NegotiationState.Stopped
import feh.util._

import scala.collection.mutable

trait AbstractNegotiation {
  val id: NegotiationId
  val issues: Set[Var]

  def scope: IdentStateVar[Set[AgentRef]]

  protected abstract class StateVar[Repr, Get, Upd](val name: String, newRepr: Repr, get: Repr => Get){
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
      val old = apply()
      val n = f(apply())
      val newRepr = upd(n, repr)
      stateVarChanged(name, old -> n)
      newRepr
    }

    def reset() = repr = newRepr
  }

  protected object VarUpdateHooks{
    type HookName = String
    type StateVarName = String
    type OldValue = Any
    type NewValue = Any
    type Change = (OldValue, NewValue)
    type Hook = Map[StateVarName, Change] => Unit

    protected var hooks: Map[HookName, Hook] = Map()

    def addHook(name: String, hook: Hook): Unit = hooks += name -> hook
    def rmHook(name: String): Unit = hooks -= name

    def runHooks(changes: Map[StateVarName, Change]): Unit = hooks.mapValues(_(changes))
  }

  protected def stateVarChanged(name: String, change: VarUpdateHooks.Change) = { /* stub */ }

  protected class IdentStateVar[T](name: String,
                                   newRepr: T,
                                   get: T => T = identity[T] _,
                                   _upd: (T,  T) => T = (t: T, _: T) => t) extends StateVar[T, T, T](name, newRepr, get){
    protected def upd = _upd
  }

  protected class OptionStateVar[T](name: String,
                                    newRepr: Option[T] = None,
                                    _upd: (T,  Option[T]) => Option[T] = (t: T, _: Option[T]) => Option(t)) extends StateVar[Option[T], T, T](name, newRepr, (_: Option[T]).getOrThrow(s"$name is not defined"))
  {
    protected def upd = _upd
    def opt = raw
    def getOrElse(t: => T) = raw getOrElse t
    def map[R](f: T => R) = raw map f
  }

  lazy val currentValues = new StateVar[mutable.HashMap[Var, Any], Map[Var, Any], Map[Var, Any]](
    "values", mutable.HashMap.empty[Var, Any], _.toMap
  ){
    protected def upd = (values, map) =>
      if(currentValuesUpdateAllowed(values)) {
        reset()
        map ++= values
      }
      else sys.error("current values update not allowed")
  }

  lazy val currentState = new IdentStateVar[NegotiationState]("state", Stopped)

  protected def currentValuesUpdateAllowed(values: Map[Var, Any]) = true

  override def toString: String = s"Negotiation(${id.name}})"
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
    lazy val currentProposal = new OptionStateVar[Lang#Proposal]("proposal")
  }

  trait HasPriority extends AbstractNegotiation{
    lazy val currentPriority = new OptionStateVar[Priority]("priority")
  }

  trait DynamicScope extends AbstractNegotiation{
    val scope = new IdentStateVar[Set[AgentRef]]("scope", Set())

    def scopeUpdated()
  }

  trait HasIterator extends AbstractNegotiation{
    lazy val currentIterator = new OptionStateVar[ProposalEngine.DomainIterator]("domain-iterator")
  }

  trait ChangeHooks extends AbstractNegotiation{
    protected object BulkChanging extends ThreadUnsafeScopedState[Boolean](false) {// no `thread locals` within actors
      var currentBulk: Map[VarUpdateHooks.StateVarName, VarUpdateHooks.Change] = Map()

      def runHooks() = {
        VarUpdateHooks.runHooks(currentBulk)
        currentBulk = Map()
      }
    }
    def bulkUpdate[R](f: => R) = BulkChanging.doWith(true)(f) $$ { _ => BulkChanging.runHooks() }

    override protected def stateVarChanged(name: String, change: VarUpdateHooks.Change) = 
      if(BulkChanging.get) BulkChanging.currentBulk += name -> change else VarUpdateHooks.runHooks(Map(name -> change))
    
    object ChangeHooks{
      def add(name: String, hook: VarUpdateHooks.Hook) = VarUpdateHooks.addHook(name, hook)
      def rm = VarUpdateHooks.rmHook _
    }
  }
}