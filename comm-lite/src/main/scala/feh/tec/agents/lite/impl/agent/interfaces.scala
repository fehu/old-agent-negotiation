package feh.tec.agents.lite.impl.agent

import feh.tec.agents.lite.{AgentCreationInterface, AgentCreationInterfaceDescriptor}

object interfaces {
  def empty = AgentCreationInterfaceDescriptor(Map())
//  lazy val priorityAndProposalBased = AgentCreationInterfaceDescriptor(Map())
//  lazy val iteratingAll = AgentCreationInterfaceDescriptor(Map())
//  lazy val initialDistinctPriority = AgentCreationInterfaceDescriptor(Map("initial-priority" -> AgentCreationInterface.InitialDistinctPriority))
}
