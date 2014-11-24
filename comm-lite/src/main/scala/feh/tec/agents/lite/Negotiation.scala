package feh.tec.agents.lite

import feh.tec.agents.lite.NegotiationState.Stopped
import feh.util._

import scala.collection.mutable

trait AbstractNegotiation {
  val id: NegotiationId
  val issues: Set[Var]

  def scope: IdentStateVar[Set[AgentRef]]

//  def statesAsString: List[String] = List(currentValues, currentState).map(_.toString)

  def report(): Map[String, Any] = states.map(s => s.name -> s.raw).toMap

  private var _states = Set.empty[StateVar[_, _, _]]
  def states = _states

  protected abstract class StateVar[Repr, Get, Upd](val name: String, newRepr: Repr, get: Repr => Get){
    private var _repr = newRepr
    protected def repr = _repr

    def raw = publicRaw(_repr)
    protected def upd: (Upd, Repr) => Repr

    def apply(): Get = get(_repr)
    def update(u: Upd): Unit = _repr = {
//        reset()
        val newRepr = upd(u, _repr)
        stateVarChanged(name, raw -> publicRaw(newRepr))
        newRepr
      }

    protected def publicRaw(in: Repr): Any = in

    def update(f: Get => Upd): Unit = {
//      reset()
      update(f(apply()))
    }

    def reset() = _repr = newRepr

    override def toString: String = s"StateVar($name = $raw)"

    _states += this
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

    def runHooks(changes: Map[StateVarName, Change]): Unit = hooks.foreach(_._2(changes))
  }

  protected def stateVarChanged(name: String, change: VarUpdateHooks.Change) = { /* stub */ }

  protected class IdentStateVar[T](name: String,
                                   newRepr: T,
                                   get: T => T = identity[T] _,
                                   _upd: (T,  T) => T = (t: T, _: T) => t) extends StateVar[T, T, T](name, newRepr, get){
    protected def upd = _upd

    override def raw: T = super.raw.asInstanceOf[T]
    override protected def publicRaw(in: T): T = in
  }

  protected class OptionStateVar[T](name: String,
                                    newRepr: Option[T] = None,
                                    _upd: (T,  Option[T]) => Option[T] = (t: T, _: Option[T]) => Option(t))
    extends StateVar[Option[T], T, T](name, newRepr, (_: Option[T]).getOrThrow(s"$name is not defined"))
  {
    protected def upd = _upd
    def opt = raw
    def getOrElse(t: => T) = repr getOrElse t
    def map[R](f: T => R) = repr map f

    override def raw: Option[T] = super.raw.asInstanceOf[Option[T]]
    override protected def publicRaw(in: Option[T]): Option[T] = in
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

    override def raw: Map[Var, Any] = super.raw.asInstanceOf[Map[Var, Any]]
    override protected def publicRaw(in: mutable.HashMap[Var, Any]): Map[Var, Any] = in.toMap
  }

  lazy val currentState = new IdentStateVar[NegotiationState]("state", Stopped)

  protected def currentValuesUpdateAllowed(values: Map[Var, Any]) = true

  override def toString: String = s"Negotiation(${id.name}, ${states.mkString(", ")}})"
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

  trait ChangingIssues extends AbstractNegotiation{
    lazy val currentIssues = new IdentStateVar[Seq[Var]]("current-issues", Nil)
  }

  trait HasProposal[Lang <: Language.ProposalBased] extends AbstractNegotiation{
    lazy val currentProposal = new OptionStateVar[Lang#Proposal]("proposal")

    //override def statesAsString: List[String] = super.statesAsString ::: List(currentProposal).map(_.toString)
  }

  trait HasPriority extends AbstractNegotiation{
    lazy val currentPriority = new OptionStateVar[Priority]("priority")

    //override def statesAsString: List[String] = super.statesAsString ::: List(currentPriority).map(_.toString)
  }

  trait DynamicScope extends AbstractNegotiation{
    val scope = new IdentStateVar[Set[AgentRef]]("scope", Set())

    def scopeUpdated()
  }

  trait HasIterator extends AbstractNegotiation{
    lazy val currentIterator = new OptionStateVar[ProposalEngine.DomainIterator]("domain-iterator")
  }

  trait HasIterators  extends HasIterator{
    self: ChangingIssues =>

    lazy val currentIterators = new IdentStateVar[Map[Set[Var], ProposalEngine.DomainIterator]]("domain-iterator", Map())
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