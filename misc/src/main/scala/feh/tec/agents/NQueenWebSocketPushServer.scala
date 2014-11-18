package feh.tec.agents

import akka.actor.{Props, ActorSystem}
import feh.tec.agents.light.Message.ProposalId
import feh.tec.agents.light._
import scala.concurrent.duration._
import feh.tec.web.{WebSocketPushServerInitialization, WebSocketPushServer}
import feh.tec.web.WebSocketPushServer.Push
import feh.tec.web.common.{WebSocketMessages, NQueenMessages}
import spray.json.JsonFormat

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

class NQueenWebSocketPushServer(neg: NegotiationId,
                                flushFrequency: FiniteDuration)
  extends WebSocketPushServer
{
  import feh.tec.web.NQueenProtocol._

  protected lazy val indexMap = mutable.HashMap.empty[AgentRef, NQueenMessages.Queen]

  protected lazy val RefNameRegex = """.+\-(\d+)""".r
  protected def addNewIndex(ref: AgentRef) = ref match {
    case AgentRef(Agent.Id(RefNameRegex(i), role), _) =>
      val q = NQueenMessages.Queen(i.toInt)
      indexMap += ref -> q
      q
  }

  lazy val timeReference = System.currentTimeMillis()
  def timeDiff(time: Long) = (timeReference - time).toInt

  def push[Msg <: NQueenMessages.Msg : JsonFormat](msg: Msg) =
    Push(msg, implicitly[JsonFormat[Msg]].asInstanceOf[JsonFormat[WebSocketMessages#Msg]])

  private def getPos(vals: Map[Var, Any], nme: String) = vals.find(_._1.name == nme).get._2.asInstanceOf[Int]
  private def getPosXY(vals: Map[Var, Any]) = getPos(vals, "x") -> getPos(vals, "y")

  protected def reportToBulkable(report: AgentReport): Option[NQueenMessages.CanBulk] = {
    report match {
      case msg@StateReport(`neg`, changes, "changes", time) =>
        val ref = msg.sender
        val q = indexMap.getOrElse(ref, addNewIndex(ref))
        val valsOpt = changes.get("values").map(_.asInstanceOf[(Map[Var, Any], Map[Var, Any])])
        val posOpt = valsOpt.map(_._2).map(getPosXY)
        val stateOpt = changes.get("state").map(_.asInstanceOf[(NegotiationState, NegotiationState)]._2.toString)

        val rep = NQueenMessages.ChangeReport(q, timeDiff(time), posOpt, stateOpt)
        Some(rep)
      case rep@MessageReport(msg, _to, time) =>
        val by = indexMap.getOrElse(rep.msg.sender, addNewIndex(rep.msg.sender))
        val to = indexMap.getOrElse(_to, addNewIndex(_to))
        def buildMessage(content: NQueenMessages.type => NQueenMessages.MessageContent) =
          NQueenMessages.Message(timeDiff(time), msg.asInstanceOf[Message.HasPriority].priority.get, content(NQueenMessages))

        val message = msg match{
          case prop@Message.Proposal(ProposalId(id), `neg`, _, vals) =>
            Some(buildMessage(_.Proposal(id.toString, getPosXY(vals))))
          case acc@Message.Accepted(`neg`, offer, _, _) =>
            Some(buildMessage(_.Response(offer.id, _.Acceptance)))
          case rej@Message.Rejected(`neg`, offer, _, _)  =>
            Some(buildMessage(_.Response(offer.id, _.Rejection)))
//          case conflict: Message.Conflict => None // todo
        }
//        val newExtra = extra collectFirst {
//          case AgentReports.WeightReport(weighted) =>
//            NQueenMessages.ReportWeight(weighted.mapValues(_.d).toList)
//        }
        message map (NQueenMessages.MessageReport(by, to, _, None))
    }
  }

  def scheduleFlush() = context.system.scheduler.schedule(flushFrequency, flushFrequency, self, FlushReports)(context.dispatcher)
  protected case object FlushReports

  protected lazy val reportsBuff = mutable.Buffer.empty[AgentReport]

  override def receive = super.receive orElse{
    // guard the reports until
    case rep@StateReport(`neg`, _, "changes", _) =>
      reportsBuff += rep
    case rep@MessageReport(msg, _, _) if msg.negotiation == neg =>
      reportsBuff += rep
    // this one is sent on connection; should be passed intact
    case AgentReport.Bulk(reports) =>
      val bulk = NQueenMessages.BulkReport(reports flatMap reportToBulkable)
      super.receive(push(bulk))
    // bulk send buff contents and clear
    case FlushReports =>
      val bulk = NQueenMessages.BulkReport(reportsBuff.toList flatMap reportToBulkable)
      reportsBuff.clear()
      super.receive(push(bulk))
    case _: SystemMessage.NegotiationFinished =>
      reportsBuff.clear()
      super.receive(push(NQueenMessages.NegotiationFinished))
//    case SystemMessage.NegotiationFinishedAutoRestart(_, delay) =>
//      reportsBuff.clear()
//      super.receive(push(NQueenMessages.NegotiationFinishedAutoRestart(delay.toMillis.toInt)))
    case NQueenMessages.Restart => super.receive(push(NQueenMessages.Restart))
//    case SharingKnowledge.ConfigurationProvenFailure(_, pos) =>
//      super.receive(push(NQueenMessages.PositionProvenFailure(getPosXY(pos))))
  }

  scheduleFlush()
}

class NQueenWebSocketPushServerBuilder(host: String, port: Int, negotiationId: NegotiationId)
                                      (implicit asys: ActorSystem)
  extends WebSocketPushServerInitialization(host, port)
{
  override def serverProps = Props(new NQueenWebSocketPushServer(negotiationId, 250 millis)) // todo: should be configurable
}
