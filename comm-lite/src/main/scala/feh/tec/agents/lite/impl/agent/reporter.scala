package feh.tec.agents.lite.impl.agent

import akka.actor.ActorRef
import feh.tec.agents.lite.{AgentRef, ReportListenerRef}
import feh.tec.agents.lite.impl.service.DefaultReportWriter


object reporter {
  lazy val default = ReportListenerRef(classOf[DefaultReportWriter], Nil)
  def forwarding(to: ActorRef*) = ReportListenerRef(classOf[DefaultReportWriter], to.toList)
}
