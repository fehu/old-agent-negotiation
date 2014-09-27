package feh.tec.agents

import akka.actor.{ActorRef, ActorSystem, Props}
import feh.tec.agents.impl.Agent.Id
import feh.tec.agents.impl.AgentReports.StateReportEntry
import feh.tec.agents.impl.NegotiationController.GenericStaticInitArgs
import feh.tec.agents.impl.agent.{GenericIteratingAgentCreation, NegotiationControllerBuilder}
import NegotiationControllerBuilder.DefaultBuildAgentArgs
import feh.tec.agents.impl._
import feh.tec.web.WebSocketPushServer.Push
import feh.tec.web.common.{NQueenMessages, WebSocketMessages}
import feh.tec.web.{WebSocketPushServer, WebSocketPushServerInitialization}
import spray.can.websocket.frame.TextFrame
import spray.json.{JsonFormat, _}

import scala.collection.mutable
import scala.concurrent.duration.{FiniteDuration, _}
import scala.reflect.ClassTag

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
  import feh.tec.web.NQueenProtocol._

  protected lazy val RefNameRegex = """(.+)\-(\d+)""".r
  def initMessage = NQueenMessages.Init(agents.map{
    case AgentRef(Id.named(RefNameRegex(name, i), _, _), _) => NQueenMessages.Queen(i.toInt) -> name
  })

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

}

class NQueenWebSocketPushServer(neg: NegotiationId, 
                                flushFrequency: FiniteDuration)
  extends WebSocketPushServer
{
  import feh.tec.web.NQueenProtocol._

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

  protected def reportToBulkable(report: AgentReport): Option[NQueenMessages.CanBulk] = {

    def getPos(vals: Map[Var, Any], nme: String) = vals.find(_._1.name == nme).get._2.asInstanceOf[Int]
    def getPosXY(vals: Map[Var, Any]) = getPos(vals, "x") -> getPos(vals, "y")

    report match {
      case AgentReports.StateReport(ref, entries, time, _) =>
        val q = indexMap.getOrElse(ref, addNewIndex(ref))
        val StateReportEntry(priority, vals, scope, acceptance, extra) = entries(neg)
        Some(NQueenMessages.StateReport(q, getPosXY(vals), priority.get, Nil, time.diff))
      case rep@AgentReports.MessageReport(_to, msg, at, extra) =>
        val by = indexMap.getOrElse(rep.msg.sender, addNewIndex(rep.msg.sender))
        val to = indexMap.getOrElse(_to, addNewIndex(_to))
        def buildMessage(content: NQueenMessages.type => NQueenMessages.MessageContent) =
          NQueenMessages.Message(msg.id.toString, msg.priority, content(NQueenMessages))

        val message = msg match{
          case prop@Message.Proposal(_, vals) =>
            Some(buildMessage(_.Proposal(getPosXY(vals))))
          case acc@Message.Accepted(_, offer) =>
            Some(buildMessage(_.Response(offer, _.Acceptance)))
          case rej@Message.Rejected(_, offer)  =>
            Some(buildMessage(_.Response(offer, _.Rejection)))
          case conflict: Message.Conflict => None // todo
        }
        val newExtra = extra collectFirst {
          case AgentReports.WeightReport(weighted) =>
            NQueenMessages.ReportWeight(weighted.mapValues(_.d).toList)
        }
        message map (NQueenMessages.MessageReport(by, to, _, at.diff, newExtra))
    }
  }

  def scheduleFlush() = context.system.scheduler.schedule(flushFrequency, flushFrequency, self, FlushReports)(context.dispatcher)
  protected case object FlushReports
  
  protected lazy val reportsBuff = mutable.Buffer.empty[AgentReport]
  
  override def receive = super.receive orElse{
    // guard the reports until
    case rep@AgentReports.StateReport(_, entries, _, _) if entries contains neg =>
      reportsBuff += rep.copy(report = Map(neg -> entries(neg)))
    case rep@AgentReports.MessageReport(_, msg, _, _) if msg.negotiation == neg =>
      reportsBuff += rep
    // this one is sent on connection; should be passed intact
    case ReportArchive.BulkReports(reports) =>
      val bulk = NQueenMessages.BulkReport(reports flatMap reportToBulkable)
      super.receive(push(bulk))
    // bulk send buff contents and clear  
    case FlushReports =>
      val bulk = NQueenMessages.BulkReport(reportsBuff.toList flatMap reportToBulkable)
      reportsBuff.clear()
      super.receive(push(bulk))
  }

  scheduleFlush()
}

class NQueenWebSocketPushServerBuilder(host: String, port: Int, negotiationId: NegotiationId)
                                      (implicit asys: ActorSystem)
  extends WebSocketPushServerInitialization(host, port)
{
  override def serverProps = Props(new NQueenWebSocketPushServer(negotiationId, 500 millis)) // todo: should be configurable
}