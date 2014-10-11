package feh.tec.agents.light.spec.dsl

import akka.actor.{Actor, ActorLogging}
import akka.event.LoggingAdapter
import akka.util.Timeout
import feh.tec.agents.light
import feh.tec.agents.light.impl.PriorityAndProposalBasedAgent
import feh.tec.agents.light.{Message, Language, Priority, NegotiationState}
import feh.tec.agents.light.spec.NegotiationSpecification.NegotiationDef
import feh.tec.agents.light.spec.{MonoDefinition, ExtendableDefinition, AgentSpecification}
import feh.tec.agents.light.spec.dsl.Agent._
import feh.tec.agents.light.spec.dsl

import scala.concurrent.duration.FiniteDuration

/** Contains definitions for macros to work with */
abstract class Agent {
  self: AgentSpecification =>

  implicit class ExtendableMonoDefinitionWrapper[Ow, Def](eDef: MonoDefinition[Ow, Def]){
    def <:=(d: Ow => Def) = eDef.DefExtension = eDef.DefExtension.copy(`override` = Some(d))
    def :=(d: Def) = eDef.DefExtension = eDef.DefExtension.copy(`override` = Some((ow: Ow) => d))
  }

  def after[Def](eDef: ExtendableDefinition.BeforeAndAfter[Def])(d: Def): Unit = ???
  def transform[Def](eDef: ExtendableDefinition.BeforeAndAfter[Def])(f: Def => Def): Unit = ???
                //eDef.AfterExtension = eDef.AfterExtension.copy(`override` = Some(f))
  def before[Def](eDef: ExtendableDefinition.BeforeAndAfter[Def])(u: => Unit): Unit = ???
                //eDef.BeforeExtension = eDef.BeforeExtension.copy(`override` = Some(u))

  def when[T](cond: WhenCondition, act: WhenAction[T]): When[T] = ???
  def when[T](cond: WhenCondition)(act: => Unit): Unit = ???
  def await[T](t: Awaitable[T], timeout: FiniteDuration): T = ???

  var state: NegotiationState = ???

  def guard(g: Guardable): Unit = ???

  def respond(msg: Msg): Unit = ???

  def log: LoggingAdapter = ???

  implicit def negotiationDefHasFold(neg: NegotiationDef): Iterable[NegotiationDef]

  implicit class NegotiationDefWrapper(neg: NegotiationDef){
    def receives(msg: Msg): WhenCondition = ???
    def receivesAll(msg: MsgSeq): WhenCondition = ???
  }
}

/*protected[dsl]*/ object Agent{
  trait When[T]{
    def apply(t: T)
  }
  trait WhenCondition
  trait WhenAction[T]

  trait Awaitable[T]

  trait Msg
  trait MsgSeq

  trait Guardable
}

trait PriorityComparing{
  self: Agent =>

  protected trait HasPriority{
    val priority: Priority
  }

  val my: HasPriority = ???
  val sender: HasPriority  = ???

  object <{ def unapply(msg: Message): Option[(Priority, Priority)] = ??? }
  object >{ def unapply(msg: Message): Option[(Priority, Priority)] = ??? }
}

trait PriorityAndProposalBased[Ag <: PriorityAndProposalBasedAgent[Lang], Lang <: Language.ProposalBased with Language.HasPriority]
  extends AgentSpecification.PriorityAndProposalBased[Ag, Lang]
  with PriorityComparing
{
  self: dsl.Agent =>

  trait NState

  case object Proposal extends Msg
  case object Accepted extends Msg with NState
  case object Rejected extends Msg with NState
  case object proposal extends Msg
  case object accepted extends Msg with NState
  case object rejected extends Msg with NState

  case object proposals extends MsgSeq
  case object responses extends MsgSeq

  case object acceptance extends Guardable
  case object rejection extends Guardable
  case object ignore

  def spamProposal: Unit = ???

  trait Constraints
  object constraints extends Constraints

  implicit class ProposalOpt(prop: Lang#Proposal){
    def breaks(c: Constraints): Boolean = ???
    def satisfies(c: Constraints): Boolean = ???
    def is(nState: NState): Unit = ???
  }

  implicit class PriorityAndProposalNegotiationDefWrapper(neg: NegotiationDef){
    def currentProposal: Message.Proposal = ???
  }

  def onProposalAcceptance //todo
  def onProposalRejection //todo
}

object PriorityAndProposalBased{
  type Lang = Language.ProposalBased with Language.HasPriority
  type Ag = PriorityAndProposalBasedAgent[Lang]

  trait HavingViews
    extends PriorityAndProposalBased[Ag, Lang]
  {
    self: dsl.Agent =>

    object view{
      def hasMinimumData: Awaitable[Unit] = ???
      def externalConstraints: Constraints = ???
    }
  }
}

trait SolutionFiltering{
  self: dsl.Agent =>

  case object messages extends MsgSeq

  object SolutionFilter{
    def accepts(msg: MsgSeq): Boolean = ???
  }
}
