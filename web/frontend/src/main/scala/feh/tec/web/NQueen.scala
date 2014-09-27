package feh.tec.web

import feh.tec.web.NQueenTemplates.QueenAcceptanceFlagStyles
import feh.tec.web.common.NQueenMessages._
import org.scalajs.dom.WebSocket
import org.scalajs.jquery.jQuery
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.{JSApp, JSON}

object NQueen extends JSApp with NQueenSocketListener{
  lazy val wsUrl = jQuery("head meta[ws]").attr("ws")
  def main() = {}

  protected var chessBoard: ChessBoard = null
  protected var queensInfo: Map[Int, QueenInfo] = null
  protected var reportArchive: ReportArchive = null
  protected var communications: QueensCommunications = null
  protected var selection: QueenInfo.Selection = null

  def initNegotiationInfo(queens: Map[Int, String]): Any = {
    reportArchive = new ReportArchive(queens.keySet)
    chessBoard = new ChessBoard(queens.size)
    communications = new QueensCommunications(reportArchive)
    selection = new QueenInfo.Selection(
      s => (communications.update _).tupled apply s.queens(queens(s.left.get), queens(s.right.get))
    )
    queensInfo = queens.map{ case (i, name) => i -> new QueenInfo(name, i, selection) }.toMap

    val containerForInfo  = jQuery("[containerFor=queen-info]")
    val containerForBoard = jQuery("[containerFor=chess-board]")

    containerForInfo append s"""<div class="queen-info">${queensInfo.map(_._2.infoList).mkString("\n")}</div>"""

    queensInfo.foreach(_._2.setCallbacks())

    jQuery("head") append QueenAcceptanceFlagStyles.generate(queens.size)
    jQuery(containerForBoard) append chessBoard.create
    jQuery(containerForInfo) append communications.create.toString()

    reportArchive.onNewStates{ (reps: Map[Int, List[StateReport]]) =>
      val latest = reps.mapValues(_.maxBy(_.at))
      chessBoard updatePositions latest.mapValues(_.position)
      queensInfo.foreach{
        case (i, q) => latest.get(i) foreach q.updateState
      }
    }
  }

  def bulkReport(report: BulkReport): Any = reportArchive.report(report)
}


trait NQueenSocketListener extends SocketConnections{

  def wsUrl: String

  protected lazy val sockets = new WebSocket(wsUrl) :: Nil

  def initNegotiationInfo(queens: Map[Int, String]): Any
  def bulkReport(report: BulkReport): Any

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
        acceptanceFlag = json.acceptanceFlag.asInstanceOf[Boolean]
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

class ReportArchive(queens: Set[Int]){

  def report(msg: BulkReport): Unit = {
    val grouped_s =
      msg.messages.toList.withFilter(_.isInstanceOf[StateReport]).map(_.asInstanceOf[StateReport]).groupBy(_.by.n)

    val grouped_m =
      msg.messages.toList.withFilter(_.isInstanceOf[MessageReport]).map(_.asInstanceOf[MessageReport]).groupBy(_.by.n)

    grouped_s foreach{ case (i, reps) => _states(i)   ++= reps}
    grouped_m foreach{ case (i, reps) => _messages(i) ++= reps}

    _onNewMessages foreach (_(grouped_m))
    _onNewStates foreach (_(grouped_s))
  }

  def states(i: Int)                             = _states(i).toSet
  def messages: Map[Int, List[MessageReport]]    = _messages.mapValues(_.toList.sortBy(_.at))
  def messages(i: Int): List[MessageReport]      = _messages(i).toList.sortBy(_.at)
  def messages(i: Set[Int]): List[MessageReport] = _messages.filterKeys(i.contains).values.flatten.toList.sortBy(_.at)

  def onNewStates(byQueen: js.Function1[Map[Int, List[StateReport]], Any]): Unit     = { _onNewStates += byQueen }
  def onNewMessages(byQueen: js.Function1[Map[Int, List[MessageReport]], Any]): Unit = { _onNewMessages += byQueen }
  def rmOnNewMessagesCallback(func: js.Function1[Map[Int, List[MessageReport]], Any]): Unit = { _onNewMessages -= func }

  protected val _states   = queens.map{ q => q -> mutable.Set.empty[StateReport]   }.toMap
  protected val _messages = queens.map{ q => q -> mutable.Set.empty[MessageReport] }.toMap

  protected val _onNewStates   = mutable.Buffer.empty[js.Function1[Map[Int, List[StateReport]], Any]]
  protected val _onNewMessages = mutable.Buffer.empty[js.Function1[Map[Int, List[MessageReport]], Any]]
}