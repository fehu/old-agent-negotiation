package feh.tec.agents.light.spec.dsl

import akka.actor.{Actor, ActorLogging}
import akka.event.LoggingAdapter
import feh.tec.agents.light
import feh.tec.agents.light.{Message, Language, Priority, NegotiationState}
import feh.tec.agents.light.spec.NegotiationSpecification.NegotiationDef
import feh.tec.agents.light.spec.{MonoDefinition, ExtendableDefinition, AgentSpecification}
import feh.tec.agents.light.spec.dsl.Agent._

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
  def await[T](t: Awaitable[T]): T = ???

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

protected[dsl] object Agent{
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

  type HasPriority = {
    def priority: Priority
  }

  def agent: HasPriority
  def sender: HasPriority

  case class <(l: Priority, r: Priority)
  case class >(l: Priority, r: Priority)
}

trait PriorityAndProposalBased
  extends AgentSpecification.PriorityAndProposalBased
  with PriorityComparing
{
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
}