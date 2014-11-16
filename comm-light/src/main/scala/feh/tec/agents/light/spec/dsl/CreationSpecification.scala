package feh.tec.agents.light.spec.dsl

import scala.concurrent.duration.FiniteDuration

trait CreationSpecification{
  self: Negotiation =>

  import feh.tec.agents.light.spec.NegotiationSpecification._

  protected type TimeoutIdent = {
    def <= (t: FiniteDuration): TimeoutDef
  }

  protected trait ConfDef
  protected trait TimeoutDef extends ConfDef

  type ChooseTimeout = {
    def initialize: TimeoutIdent
    def start: TimeoutIdent
    def stop: TimeoutIdent
    def reset: TimeoutIdent
  }

  def spawn:{
    def agents(count: (AgentDef, Int))
    def agents(count: (String, Int))
    def agent(ag: AgentDef)
  } = stub

  def configure(c: ConfDef*) = stub
  def timeout: ChooseTimeout = stub

//  implicit class TimingDefWrapper(f: ChooseTimeout => TimeoutIdent){
//    def >>(time: FiniteDuration): TimeoutDef = stub
//  }
}
