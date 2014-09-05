package feh.tec.agents

import akka.actor.{Props, ActorSystem}
import feh.tec.agents.Message.AutoId
import feh.tec.agents.impl.NegotiationController.GenericStaticInitArgs
import feh.tec.agents.impl.NegotiationControllerBuilder.DefaultBuildAgentArgs
import feh.tec.agents.impl.agent.AgentBuilder
import feh.tec.agents.impl.{ReportsPrinter, ReportArchive, GenericIteratingAgentCreation, NegotiationControllerBuilder}

import scala.reflect.ClassTag

class ControllerBuilder[AgImpl <: GenericIteratingAgentCreation[_]]
                       (implicit acSys: ActorSystem, defaultAgentImplementation: ClassTag[AgImpl])
  extends NegotiationControllerBuilder.Default[AgImpl]
{

  override def systemAgents = super.systemAgents ++ Map(
    (AgentBuilder.SystemArgs0Service, scala.reflect.classTag[ReportArchiveGUI]) -> 1
  )

  override protected def props(arg: GenericStaticInitArgs[DefaultBuildAgentArgs]) = Props(classOf[Controller], arg)
}

//case class ControllerArgs(genericStaticInit: GenericStaticInitArgs[DefaultBuildAgentArgs], reportsGui: AgentRef)

class Controller(arg: GenericStaticInitArgs[DefaultBuildAgentArgs]) extends NegotiationControllerBuilder.DefaultController(arg){
  def reportsGui = getSystemAgent(ReportArchive)

  override def start() = {
    super.start()
    reportingTo.ref ! ReportsPrinter.Forward(reportsGui)
  }

  override def processSys = super.processSys orElse{
    case Controller.ShowReportGui(ag) => reportsGui.ref ! ReportArchiveGUI.ShowReports(ag)
  }
}

object Controller{
  case class ShowReportGui(ag: AgentRef) extends SystemMessage with AutoId
}