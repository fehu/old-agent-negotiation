package feh.tec.agents.light.impl.agent

import feh.tec.agents.light.{AgentCreationInterface, AgentCreationInterfaceDescriptor}

object interfaces {
  def empty = AgentCreationInterfaceDescriptor(Map())
//  lazy val priorityAndProposalBased = AgentCreationInterfaceDescriptor(Map())
//  lazy val iteratingAll = AgentCreationInterfaceDescriptor(Map())
//  lazy val initialDistinctPriority = AgentCreationInterfaceDescriptor(Map("initial-priority" -> AgentCreationInterface.InitialDistinctPriority))
}
