package feh.tec.agents.lite.spec.dsl

import feh.tec.agents.lite.{Var, NegotiationId}
import feh.tec.agents.lite.impl.NegotiationEnvironmentController

import scala.concurrent.duration.FiniteDuration

trait CreationSpecification{
  self: Negotiation =>

  import feh.tec.agents.lite.spec.NegotiationSpecification._

  protected type TimeoutIdent = {
    def <= (t: FiniteDuration): TimeoutDef
  }

  protected trait ConfDef
  protected trait TimeoutDef extends ConfDef

  type Reason = String
  type Controller = NegotiationEnvironmentController // todo
  type Values = Seq[Map[Var, Any]]

  type ChooseTimeout = {
    def initialize: TimeoutIdent
    def start: TimeoutIdent
    def stop: TimeoutIdent
    def reset: TimeoutIdent
    def `response delay`: TimeoutIdent
  }

  type ChooseWhen = {
    def finished(action: Controller => (NegotiationId, Values) => Any)
    def failed(action: Controller => (NegotiationId, Reason) => Any)
  }

  def spawn:{
    def agents(count: (AgentDef, Int))
    def agents(count: (String, Int))
    def agent(ag: AgentDef)
  } = stub

  def configure(c: ConfDef*) = stub
  def timeout: ChooseTimeout = stub

  def when: ChooseWhen = ???

}
