package feh.tec.agents.light.spec

import akka.actor.Props

trait AgentSpecification {
  type Agent
  type BuildArgs

  def build(args: BuildArgs): Props
}

object AgentSpecification{
  trait PriorityAndProposalBased extends AgentSpecification{

  }
}