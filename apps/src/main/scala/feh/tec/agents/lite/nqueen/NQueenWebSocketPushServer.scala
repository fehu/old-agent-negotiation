package feh.tec.agents.lite.nqueen

import akka.actor.{ActorSystem, Props}
import feh.tec.agents.lite.Message.ProposalId
import feh.tec.agents.lite._
import feh.tec.web.WebSocketPushServer.Push
import feh.tec.web.common.{NQueenMessages, WebSocketMessages}
import feh.tec.web.{WebSocketPushServer, WebSocketPushServerCreation}
import spray.json.JsonFormat

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

class NQueenWebSocketPushServer(neg: NegotiationId,
                                flushFrequency: FiniteDuration,
                                liteReporting: Boolean)
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

  val timeReference = System.nanoTime()
  def timeDiff(time: Long) = (time - timeReference).toInt

  def push[Msg <: NQueenMessages.Msg : JsonFormat](msg: Msg) =
    Push(msg, implicitly[JsonFormat[Msg]].asInstanceOf[JsonFormat[WebSocketMessages#Msg]])

  private def getPosOrElse(vals: Map[Var, Any], nme: String, orElse: Int) =
    vals.find(_._1.name == nme).map(_._2.asInstanceOf[Int]).getOrElse(orElse)
  private def getPosXY(vals: Map[Var, Any]) = getPosOrElse(vals, "x", 1) -> getPosOrElse(vals, "y", 1)

  protected def reportToBulkable(report: AgentReport): Option[NQueenMessages.CanBulk] = {
    report match {
      case msg@StateReport(`neg`, changes, "changes", time) =>
        val ref = msg.sender
        val q = indexMap.getOrElse(ref, addNewIndex(ref))
        val valsOpt = changes.get("values").map(_.asInstanceOf[(Map[Var, Any], Map[Var, Any])])
        val posOpt = valsOpt.map(_._2).map(getPosXY)
        val stateOpt = changes.get("state").map(_.asInstanceOf[(NegotiationState, NegotiationState)]._2.toString)
        val priorityOpt = changes.get("priority").map(_.asInstanceOf[(Option[Priority], Option[Priority])]._2).flatten.map(_.get)

        val rep = NQueenMessages.ChangeReport(q, timeDiff(time), posOpt, stateOpt, priorityOpt)
        Some(rep)
      case msg@StateReport(`neg`, vals, "by demand", time) =>
        val ref = msg.sender
        val q = indexMap.getOrElse(ref, addNewIndex(ref))

        val rep = NQueenMessages.ChangeReport(q, timeDiff(time),
          if(vals.contains("values")) Some(getPosXY(vals("values").asInstanceOf[Map[Var, Any]])) else None,
          Some(vals("state").asInstanceOf[NegotiationState].toString),
          Some(vals("priority").asInstanceOf[Option[Priority]]).flatten.map(_.get)
          )
        Some(rep)
      case rep@MessageReport(msg, _to, extra, time) =>
        val by = indexMap.getOrElse(rep.msg.sender, addNewIndex(rep.msg.sender))
        val to = indexMap.getOrElse(_to, addNewIndex(_to))
        def buildMessage(content: NQueenMessages.type => NQueenMessages.MessageContent) =
          NQueenMessages.Message(timeDiff(time), msg.asInstanceOf[Message.HasPriority].priority.get, content(NQueenMessages))

        val message = PartialFunction.condOpt(msg){
          case prop@Message.Proposal(ProposalId(id), `neg`, _, vals) =>
            buildMessage(_.Proposal(id.toString, getPosXY(vals)))
          case acc@Message.Accepted(`neg`, offer, _, vals, _) =>
            buildMessage(_.Response(offer.id, getPosXY(vals), _.Acceptance))
          case rej@Message.Rejected(`neg`, offer, _, vals)  =>
            buildMessage(_.Response(offer.id, getPosXY(vals), _.Rejection))
//          case conflict: Message.Conflict => None // todo
        }
//        val newExtra = extra collectFirst {
//          case AgentReports.WeightReport(weighted) =>
//            NQueenMessages.ReportWeight(weighted.mapValues(_.d).toList)
//        }
        message map (NQueenMessages.MessageReport(by, to, _, PartialFunction.condOpt(extra)(Map())))
    }
  }

  def scheduleFlush() = context.system.scheduler.schedule(flushFrequency, flushFrequency, self, FlushReports)(context.dispatcher)
  protected case object FlushReports

  protected lazy val reportsBuff = mutable.Buffer.empty[AgentReport]

  override def receive = super.receive orElse{
    // guard the reports until
    case rep@StateReport(`neg`, _, "changes", _) =>
      reportsBuff += rep
    case rep@MessageReport(msg, _, _, _) if msg.negotiation == neg =>
      reportsBuff += rep
    // this one is sent on connection; should be passed intact
    case rep@StateReport(`neg`, _, "by demand", _) => super.receive(push(reportToBulkable(rep).get))
//    case AgentReport.Bulk(reports) =>
//      val bulk = NQueenMessages.BulkReport(reports flatMap reportToBulkable)
//      super.receive(push(bulk))
    // bulk send buff contents and clear
    case FlushReports =>
      val preBulk = NQueenMessages.BulkReport(reportsBuff.toList flatMap reportToBulkable)

      val bulk =
        if(liteReporting) preBulk.copy(
          preBulk.messages.withFilter(_.reportsState).map(_.asInstanceOf[NQueenMessages.ChangeReport])
            .groupBy(rep => rep.by -> rep.position.map(_ => "position").orElse(rep.state.map(_ => "state")).getOrElse(""))
            .mapValues(_.maxBy(_.at)).values.toSeq
        )
        else preBulk

      reportsBuff.clear()
      super.receive(push(bulk))
    case _: SystemMessage.NegotiationFinished =>
      reportsBuff.clear()
      super.receive(push(NQueenMessages.NegotiationFinished))
//    case SystemMessage.NegotiationFinishedAutoRestart(_, delay) =>
//      reportsBuff.clear()
//      super.receive(push(NQueenMessages.NegotiationFinishedAutoRestart(delay.toMillis.toInt)))
    case NQueenMessages.Restart => super.receive(push(NQueenMessages.Restart))
    case _: Fallback.FallbackRequest => // do nothing
    case _: Fallback.IWillMove       => // do nothing
  }

  scheduleFlush()
}

class NQueenWebSocketPushServerBuilder(host: String, port: Int,
                                       negotiationId: NegotiationId,
                                       flushFrequency: FiniteDuration,
                                       onConnection: WebSocketPushServer.OnConnection
                                        )
                                      (implicit asys: ActorSystem)
  extends WebSocketPushServerCreation(host, port, "NQueenWebServer", onConnection)
{
  override def serverProps = Props(new NQueenWebSocketPushServer(negotiationId, flushFrequency, liteReporting = true))
}
