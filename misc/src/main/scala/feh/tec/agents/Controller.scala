package feh.tec.agents

import akka.actor.{ActorRef, ActorSystem, Props}
import feh.tec.agents.impl.Agent.Id
import feh.tec.agents.impl.AgentReports.StateReportEntry
import feh.tec.agents.impl.NegotiationController.GenericStaticInitArgs
import feh.tec.agents.impl.NegotiationControllerBuilder.DefaultBuildAgentArgs
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
      Right(() => reportingTo.ref ! ReportArchive.BulkAndForward(web.ref))
    ))
  }

}

class NQueenWebSocketPushServer(neg: NegotiationId, 
                                rescheduleUnfoundMsg: FiniteDuration, 
                                unfoundMsgRetries: Int,
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

  protected val proposalsMap = mutable.HashMap.empty[Message.Id, (Int, Int)]
  protected val rescheduelCount = mutable.HashMap.empty[Message.Id, Int]

  protected def reportToBulkable(report: AgentReport): Option[NQueenMessages.CanBulk] = {

    def getPos(vals: Map[Var, Any], nme: String) = vals.find(_._1.name == nme).get._2.asInstanceOf[Int]

    report match {
      case AgentReports.StateReport(ref, entries, time, _) =>
        val q = indexMap.getOrElse(ref, addNewIndex(ref))
        val StateReportEntry(priority, vals, scope, extra) = entries(neg)
        val position = getPos(vals, "x") -> getPos(vals, "y")
        Some(NQueenMessages.StateReport(q, position, priority.get, Nil, time.diff))
      case rep@AgentReports.MessageReport(_to, msg, at) =>
        val by = indexMap.getOrElse(rep.msg.sender, addNewIndex(rep.msg.sender))
        val to = indexMap.getOrElse(_to, addNewIndex(_to))
        val message = msg match{
          case prop@Message.Proposal(_, vals) =>
            val position = getPos(vals, "x") -> getPos(vals, "y")
//            log.info(s"Proposal ${prop.id}")
            proposalsMap += prop.id -> position
            Some(NQueenMessages.Message(prop.priority.get, position, NQueenMessages.Proposal))
          case acc@Message.Accepted(_, offer) if proposalsMap contains offer =>
            Some(NQueenMessages.Message(acc.priority.get, proposalsMap(offer), NQueenMessages.Acceptance))
          case rej@Message.Rejected(_, offer) if proposalsMap contains offer =>
            Some(NQueenMessages.Message(rej.priority.get, proposalsMap(offer), NQueenMessages.Rejection))
          case acc@Message.Accepted(_, offer) =>
            if(rescheduelCount.get(offer).exists(_ >= unfoundMsgRetries)) sys.error(s"no offer $offer found for $acc")
            rescheduelCount(offer) = rescheduelCount.getOrElse(offer, 0) + 1
            context.system.scheduler.scheduleOnce(rescheduleUnfoundMsg, self, acc)(context.dispatcher)
            None
          case rej@Message.Rejected(_, offer) =>
            if(rescheduelCount.get(offer).exists(_ >= unfoundMsgRetries)) sys.error(s"no offer $offer found for $rej")
            rescheduelCount(offer) = rescheduelCount.getOrElse(offer, 0) + 1
            context.system.scheduler.scheduleOnce(rescheduleUnfoundMsg, self, rej)(context.dispatcher)
            None
        }
        message map (NQueenMessages.MessageReport(by, to, _, at.diff))
    }
  }

  def scheduleFlush() = context.system.scheduler.schedule(flushFrequency, flushFrequency, self, FlushReports)(context.dispatcher)
  protected case object FlushReports
  
  protected lazy val reportsBuff = mutable.Buffer.empty[AgentReport]
  
  override def receive = super.receive orElse{
    // guard the reports until
    case rep@AgentReports.StateReport(_, entries, _, _) if entries contains neg =>
      reportsBuff += rep.copy(report = Map(neg -> entries(neg)))
    case rep@AgentReports.MessageReport(_, msg, _) if msg.negotiation == neg =>
      reportsBuff += rep
    // this one is sent on connection; should be passed intact
    case ReportArchive.BulkReports(reports) =>
      val bulk = NQueenMessages.BulkReport(reports flatMap reportToBulkable)
      super.receive(push(bulk))
    // bulk send buff contents and clear  
    case FlushReports =>
      val bulk = NQueenMessages.BulkReport(reportsBuff.toList flatMap reportToBulkable)
      log.info(s"bulk = $bulk")
      reportsBuff.clear()
      super.receive(push(bulk))
  }

  scheduleFlush()
}

class NQueenWebSocketPushServerBuilder(host: String, port: Int, negotiationId: NegotiationId)
                                      (implicit asys: ActorSystem)
  extends WebSocketPushServerInitialization(host, port)
{
  override def serverProps = Props(new NQueenWebSocketPushServer(negotiationId, 5 millis, 5, 500 millis)) // todo: should be configurable
}