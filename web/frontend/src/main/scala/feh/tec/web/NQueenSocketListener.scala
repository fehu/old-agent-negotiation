package feh.tec.web

import feh.tec.web.common.NQueenMessages._
import org.scalajs.dom.WebSocket

import scala.scalajs.js
import scala.scalajs.js.{Dynamic, JSON}

trait NQueenSocketListener extends SocketConnections{

  def wsUrl: String

  protected lazy val sockets = new WebSocket(wsUrl) :: Nil

  def initNegotiationInfo(queens: Map[Int, String]): Any
  def bulkReport(report: BulkReport): Any

  protected def restartSupport = new NQueenRestart
  protected def chessBoard: ChessBoard

  def onMessage: PartialFunction[js.Any, Unit] = {
    case msg: js.prim.String =>
      val json = JSON.parse(msg)
      //      global.console.log(json)
      json.$t.asInstanceOf[String] match {
        case "Init" =>
          val queensInit = json.queens.asInstanceOf[js.Array[js.Array[_]]].map(
            (arr: js.Array[_]) =>
              arr(0).asInstanceOf[js.Dynamic].n.asInstanceOf[Int] -> arr(1).asInstanceOf[String]
          )
          initNegotiationInfo(queensInit.toMap)
        // ignore empty
        case "BulkReport" if ! json.messages.asInstanceOf[js.Array[_]].isEmpty =>
          val bulkable = json.messages.asInstanceOf[js.Array[js.Dynamic]].flatMap( getBulkable(_: js.Dynamic) )
          bulkReport(BulkReport(bulkable))
        case "BulkReport" =>
        case "NegotiationFinished" => js.eval("alert('Negotiation Finished')")
        case "NegotiationFinishedAutoRestart" => restartSupport.negotiationFinishedAndWillRestart(json.delay.asInstanceOf[Int])
        case "Restart" =>
          restartSupport.restart()
          chessBoard.resetFailedPositions()
        case "PositionProvenFailure" =>
          val arr = json.pos.asInstanceOf[js.Array[Int]]
          chessBoard.positionProvenFailure(arr(0) -> arr(1))
      }
  }

  protected def getBulkable(json: js.Dynamic) = PartialFunction.condOpt(json.$t.asInstanceOf[String]){
    case "StateReport" =>
      StateReport(
        by = Queen(json.by.n.asInstanceOf[Int]),
        position = getPosition(json),
        priority = json.priority.asInstanceOf[Int],
        proposalAcceptance = json.proposalAcceptance.asInstanceOf[js.Array[js.Array[_]]].map(
          (x: js.Array[_]) => Queen(x(0).asInstanceOf[js.Dynamic].n.asInstanceOf[Int]) -> x(1).asInstanceOf[Boolean]
        ),
        at = json.at.asInstanceOf[Int],
        acceptanceFlag = json.acceptanceFlag.asInstanceOf[Boolean],
        topPriorityFlag = json.topPriorityFlag.asInstanceOf[Boolean]
      )
    case "MessageReport" =>
      MessageReport(
        by = Queen(json.by.n.asInstanceOf[Int]),
        to = Queen(json.to.n.asInstanceOf[Int]),
        msg = Message(
          id = json.msg.id.asInstanceOf[String],
          priority = json.msg.priority.asInstanceOf[Int],
          content = json.msg.content.$t.asInstanceOf[String] match{
            case "Proposal" => Proposal(getPosition(json.msg.content))
            case "Response" => Response(json.msg.content.proposal.asInstanceOf[String],
              json.msg.content.tpe.asInstanceOf[String] match{
                case "Acceptance" => Acceptance
                case "Rejection"  => Rejection
              })
          }
        ),
        at = json.at.asInstanceOf[Int],
        extra = getMessageReportExtra(json.extra)
      )
  }

  protected def getMessageReportExtra(json: js.Dynamic): Option[MessageExtraReport] = json match {
    case j if js.isUndefined(j)=> None
    case weighted if !js.isUndefined(json.weight) => Option(ReportWeight(
      json.weight.asInstanceOf[js.Array[js.Array[_]]].map{
        (arr: js.Array[_]) => Some(arr(0).asInstanceOf[Boolean]) -> arr(1).asInstanceOf[Double]
      }))
  }

  private def getPosition(dyn: js.Dynamic) = dyn.position.asInstanceOf[js.Array[Int]](0) -> dyn.position.asInstanceOf[js.Array[Int]](1)
}
