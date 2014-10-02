package feh.tec.agents

import akka.actor.{ActorRef, ActorSystem, Props}
import feh.tec.agents.impl.ProposalEngine.SharingKnowledge
import feh.util._
import feh.tec.agents.impl.Agent.Id
import feh.tec.agents.impl.NegotiationController.GenericStaticInitArgs
import feh.tec.agents.impl._
import feh.tec.agents.impl.agent.NegotiationControllerBuilder.DefaultBuildAgentArgs
import feh.tec.agents.impl.agent.{GenericIteratingAgentCreation, NegotiationControllerBuilder}
import feh.tec.web.WebSocketPushServer
import feh.tec.web.common.NQueenMessages
import spray.can.websocket.frame.TextFrame
import spray.json._

import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

class ControllerBuilder[AgImpl <: GenericIteratingAgentCreation[_]](web: => WebSocketInterface, restartDelay: FiniteDuration)
                       (implicit acSys: ActorSystem, defaultAgentImplementation: ClassTag[AgImpl])
  extends NegotiationControllerBuilder.Default[AgImpl]
{
  cb =>

  override protected def props(arg: GenericStaticInitArgs[DefaultBuildAgentArgs]) = Props(new Controller(arg, web) with AutoRestart{
    def restartDelay = cb.restartDelay
  })
}

case class WebSocketInterface(ref: ActorRef)

class Controller(arg: GenericStaticInitArgs[DefaultBuildAgentArgs], web: WebSocketInterface)
  extends NegotiationControllerBuilder.DefaultController(arg)
{
  import feh.tec.web.NQueenProtocol._

  protected lazy val RefNameRegex = """(.+)\-(\d+)""".r
  def initMessage = NQueenMessages.Init(agents.map{
    case AgentRef(Id.named(RefNameRegex(name, i), _, _), _) => NQueenMessages.Queen(i.toInt) -> name
  })

  override def processSys = super.processSys orElse{
    case fail: SharingKnowledge.ConfigurationProvenFailure => web.ref ! fail
  }

  override def start() = {
    super.start()

    web.ref ! WebSocketPushServer.OnConnection(List(
      Left(TextFrame(initMessage.toJson.toString())),
      Right(() => {
        reportingTo.ref ! ReportArchive.BulkAndForward(web.ref)
//        reportingTo.ref ! ReportArchive.Print(true)

      })
    ))
  }

  override def restart() = {
    super.restart()
    web.ref ! NQueenMessages.Restart
  }

  protected def negotiationFinishedMessage(neg: NegotiationId): SystemMessage = SystemMessage.NegotiationFinished(neg)

  override def negotiationIsFinished(neg: NegotiationId) = {
    super.negotiationIsFinished(neg)
    web.ref ! negotiationFinishedMessage(neg)
  }
}

trait AutoRestart extends Controller{
  def restartDelay: FiniteDuration

  override def negotiationIsFinished(neg: NegotiationId) = {
    super.negotiationIsFinished(neg)
    context.system.scheduler.scheduleOnce(restartDelay, self, SystemMessage.Start())
  }

  override protected def negotiationFinishedMessage(neg: NegotiationId) =
    SystemMessage.NegotiationFinishedAutoRestart(neg, restartDelay)
}