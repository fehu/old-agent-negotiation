package feh.tec.agents.lite.impl.service

import java.io.File

import com.typesafe.config.{ConfigFactory, Config}
import feh.tec.agents.lite.impl.NegotiationEnvironmentController
import feh.util.Path

object SupportBundle{
  case class Config(resource: Path)
}

trait SupportBundle{
  self: NegotiationEnvironmentController =>

  def configInfo: SupportBundle.Config
  lazy val config = ConfigFactory.load(configInfo.resource.mkString(File.separator))

  def controllerKey(key: String) = s"controller.$key"
}

trait ReportPrinterSupportBundle extends SupportBundle
  with DefaultReportWriterControllerSupport
{
  self: NegotiationEnvironmentController =>

  lazy val reportFile: File = new File(config.getString(controllerKey("report.file")))

  if(!reportFile.exists()) reportFile.createNewFile()

  log.debug(s"reportFile = $reportFile, ${reportFile.exists()}")
}
