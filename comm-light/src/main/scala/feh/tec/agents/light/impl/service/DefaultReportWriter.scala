package feh.tec.agents.light.impl.service

import java.io.File
import java.text.DateFormat
import java.util.Date

import akka.actor.{ActorLogging, Props}
import feh.tec.agents.light._
import feh.tec.agents.light.impl.NegotiationEnvironmentController

trait DefaultReportPrinter extends ReportPrinter{
  protected lazy val DateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")

  def print(r: AgentReport): String = s"[${DateFormat.format(new Date(r.at))}] $r"
}

class DefaultReportWriter(val writeTo: File) extends ReportWriter with DefaultReportPrinter with ReportForwarder with ActorLogging{
  val name = DefaultReportWriter.Name
  override val role = DefaultReportWriter.Role
  log.debug("DefaultReportWriter started")
}

object DefaultReportWriter{
  val Name = "DefaultReportWriter"
  val Role = SystemRole("ReportListener")
  lazy val Id = Agent.Id(Name, Role)
}

trait DefaultReportWriterControllerSupport extends ReportListenerControllerSupport{
  self: NegotiationEnvironmentController =>

  val reportFile: File

  implicit object DefaultReportWriterBuilder extends ReportListenerBuilder[DefaultReportWriter]{
    def build(clazz: Class[DefaultReportWriter]) = AgentRef(DefaultReportWriter.Id, asys.actorOf(Props(new DefaultReportWriter(reportFile))))
  }
}