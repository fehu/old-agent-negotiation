package feh.tec.agents.light.impl.agent

import feh.tec.agents.light.ReportListenerRef
import feh.tec.agents.light.impl.service.DefaultReportWriter


object reporter {
  lazy val default = ReportListenerRef(classOf[DefaultReportWriter])
}
