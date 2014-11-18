package feh.tec.agents.light.impl.agent

import akka.actor.ActorRef
import feh.tec.agents.light.{AgentRef, ReportListenerRef}
import feh.tec.agents.light.impl.service.DefaultReportWriter


object reporter {
  lazy val default = ReportListenerRef(classOf[DefaultReportWriter], Nil)
  def forwarding(to: ActorRef*) = ReportListenerRef(classOf[DefaultReportWriter], to.toList)
}
