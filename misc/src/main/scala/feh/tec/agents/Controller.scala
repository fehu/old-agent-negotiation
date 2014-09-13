package feh.tec.agents

import akka.actor.{ActorRef, ActorSystem, Props}
import feh.tec.agents.impl.Agent.Id
import feh.tec.agents.impl.AgentReports.StateReportEntry
import feh.tec.agents.impl.NegotiationController.GenericStaticInitArgs
import feh.tec.agents.impl.NegotiationControllerBuilder.DefaultBuildAgentArgs
import feh.tec.agents.impl._
import feh.tec.web.common.NQueenMessages.Init
import feh.tec.web.{WebSocketPushServerInitialization, WebSocketServerInitialization, NQueenProtocol, WebSocketPushServer}
import feh.tec.web.WebSocketPushServer.Push
import feh.tec.web.common.{WebSocketMessages, NQueenMessages}
import spray.can.Http.Bind
import spray.can.websocket.frame.{TextFrame, Frame}
import spray.json.JsonFormat
import scala.collection.mutable
import scala.reflect.ClassTag
import spray.json._

class ControllerBuilder[AgImpl <: GenericIteratingAgentCreation[_]](web: => WebSocketInterface)
                       (implicit acSys: ActorSystem, defaultAgentImplementation: ClassTag[AgImpl])
  extends NegotiationControllerBuilder.Default[AgImpl]
{

  override protected def props(arg: GenericStaticInitArgs[DefaultBuildAgentArgs]) = Props(new Controller(arg, web))
}

case class WebSocketInterface(ref: ActorRef)

class Controller(arg: GenericStaticInitArgs[DefaultBuildAgentArgs], web: WebSocketInterface)
  extends NegotiationControllerBuilder.DefaultController(arg)
{
  import NQueenProtocol._

  protected lazy val RefNameRegex = """(.+)\-(\d+)""".r
  def initMessage = NQueenMessages.Init(agents.map{
    case AgentRef(Id.named(RefNameRegex(name, i), _, _), _) => NQueenMessages.Queen(i.toInt) -> name
  })

  override def start() = {
    super.start()
    reportingTo.ref ! ReportsPrinter.Forward(web.ref)
    reportingTo.ref ! ReportsPrinter.Silent(true)
    web.ref ! WebSocketPushServer.OnConnection(TextFrame(initMessage.toJson.toString()))
  }

}

class NQueenWebSocketPushServer(neg: NegotiationId)
  extends WebSocketPushServer
{
  import NQueenProtocol._

  protected lazy val indexMap = mutable.HashMap.empty[AgentRef, NQueenMessages.Queen]

  protected lazy val RefNameRegex = """.+\-(\d+)""".r
  protected def addNewIndex(ref: AgentRef) = ref match {
    case AgentRef(Id.named(RefNameRegex(i), role, uuid), _) =>
      val q = NQueenMessages.Queen(i.toInt)
      indexMap += ref -> q
      q
  }

  def push[Msg <: NQueenMessages.Msg : JsonFormat](msg: Msg) =
    Push(msg, implicitly[JsonFormat[Msg]].asInstanceOf[JsonFormat[WebSocketMessages#Msg]])

  override def receive = super.receive orElse{
    case AgentReports.StateReport(ref, entries, _) if entries contains neg =>
      val q = indexMap.getOrElse(ref, addNewIndex(ref))
      val StateReportEntry(priority, vals, scope, extra) = entries(neg)
      def getPos(nme: String) = vals.find(_._1.name == nme).get._2.asInstanceOf[Int]
      val position = getPos("x") -> getPos("y")
      val report = NQueenMessages.StateReport(q, position, priority.get, Nil)

      super.receive(push(report))
  }
}

class NQueenWebSocketPushServerBuilder(host: String, port: Int, negotiationId: NegotiationId)
                                      (implicit asys: ActorSystem)
  extends WebSocketPushServerInitialization(host, port)
{
  override def serverProps = Props(new NQueenWebSocketPushServer(negotiationId))
}