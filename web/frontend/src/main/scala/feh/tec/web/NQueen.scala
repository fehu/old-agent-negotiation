package feh.tec.web

import com.typesafe.config.ConfigFactory
import feh.tec.web.common.NQueenMessages._
import feh.tec.web.common.WebsocketConf
import feh.tec.web.util.GenTemplate
import feh.util.PathSelector
import feh.util.file._
import org.scalajs.dom.{WebSocket, document}
import org.scalajs.jquery.jQuery
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.{JSApp, JSON}
import scala.xml.NodeSeq

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

    jQuery(containerForBoard) append chessBoard.create
    jQuery(containerForInfo) append communications.create

    reportArchive.onNewState{ (s: StateReport) =>
      chessBoard.updatePositions(s)
      queensInfo.foreach(_._2.updateState(s))
    }
  }

  def stateReport(report: StateReport) = reportArchive.report(report)
  def messageReport(report: MessageReport): Any = reportArchive.report(report)
  def bulkReport(report: BulkReport): Any = reportArchive.report(report)
}

class NQueenTemplate extends GenTemplate("n-queen", GenTemplate.classOf(NQueen)) with WebsocketConf{
  override def templateBody: NodeSeq =
    <div containerFor="chess-board"/> :: <div containerFor="queen-info"/> :: Nil

  override def templateHead: NodeSeq = <meta ws={readWsConfig()} ></meta>

  override def css: PathSelector = "n-queen".p.selectAll()

  protected def readWsConfig(): String = wsConf.front.url("n-queen")

//  protected lazy val conf = ConfigFactory.load("websocket.conf")
}

trait NQueenSocketListener extends SocketConnections{

  def wsUrl: String

  protected lazy val sockets = new WebSocket(wsUrl) :: Nil

  def initNegotiationInfo(queens: Map[Int, String]): Any
  def stateReport(report: StateReport): Any
  def messageReport(report: MessageReport): Any
  def bulkReport(report: BulkReport): Any

  def onMessage: PartialFunction[js.Any, Unit] = {
    case msg: js.prim.String =>
      val json = JSON.parse(msg)
//      global.console.log(json)
      json.$t.asInstanceOf[String] match {
        case "StateReport"   => stateReport(getBulkable(json).get.asInstanceOf[StateReport])
        case "MessageReport" => messageReport(getBulkable(json).get.asInstanceOf[MessageReport])
        case "Init" =>
          val queensInit = json.queens.asInstanceOf[js.Array[js.Array[_]]].map(
            (arr: js.Array[_]) =>
              arr(0).asInstanceOf[js.Dynamic].n.asInstanceOf[Int] -> arr(1).asInstanceOf[String]
          )
          initNegotiationInfo(queensInit.toMap)
        case "BulkReport" =>
          bulkReport(BulkReport(json.messages.asInstanceOf[js.Array[js.Dynamic]].flatMap( getBulkable(_: js.Dynamic) )))
      }
  }

  protected def getBulkable(json: js.Dynamic) = PartialFunction.condOpt(json.$t.asInstanceOf[String]){
    case "StateReport" =>
      StateReport(
        of = Queen(json.of.n.asInstanceOf[Int]),
        position = getPosition(json),
        priority = json.priority.asInstanceOf[Int],
        proposalAcceptance = json.proposalAcceptance.asInstanceOf[js.Array[js.Array[_]]].map(
          (x: js.Array[_]) => Queen(x(0).asInstanceOf[js.Dynamic].n.asInstanceOf[Int]) -> x(1).asInstanceOf[Boolean]
        ),
        at = json.at.asInstanceOf[Int]
      )
    case "MessageReport" =>
      MessageReport(
        by = Queen(json.by.n.asInstanceOf[Int]),
        to = Queen(json.to.n.asInstanceOf[Int]),
        msg = Message(json.msg.priority.asInstanceOf[Int], getPosition(json.msg), json.msg.tpe.asInstanceOf[String] match {
          case "Proposal"   => Proposal
          case "Acceptance" => Acceptance
          case "Rejection"  => Rejection
        }),
        at = json.at.asInstanceOf[Int]
      )
  }

  private def getPosition(dyn: js.Dynamic) = dyn.position.asInstanceOf[js.Array[Int]](0) -> dyn.position.asInstanceOf[js.Array[Int]](1)
}

class ReportArchive(queens: Set[Int]){
  def report(state: StateReport): Unit = {
    _states(state.of.n) += state -> state.at
    _onNewState   foreach (_.apply(state))
  }
  def report(msg: MessageReport): Unit = {
    _messages(msg.by.n) += msg -> msg.at
    _onNewMessage foreach (_.apply(msg))
  }
  def report(msg: BulkReport): Unit = msg.messages foreach {
    case rep: StateReport    => report(rep)
    case rep: MessageReport  => report(rep)
  }

  def states(i: Int)                              = _states(i).sortBy(_._2).map(_._1).toList
  def messages(i: Int): List[MessageReport]       = _messages(i).sortBy(_._2).map(_._1).toList
  def messages(i: Set[Int]): List[MessageReport]  = _messages.filterKeys(i.contains).values.flatten
                                                      .toList.sortBy(_._2).map(_._1)

  def onNewState(func: js.Function1[StateReport, Any]): Unit     = { _onNewState += func }
  def onNewMessage(func: js.Function1[MessageReport, Any]): Unit = { _onNewMessage += func }
  def rmOnNewMessage(func: js.Function1[MessageReport, Any]): Unit = { _onNewMessage -= func }

  protected val _states   = queens.map{ q => q -> mutable.Buffer.empty[(StateReport, Long)]   }.toMap
  protected val _messages = queens.map{ q => q -> mutable.Buffer.empty[(MessageReport, Long)] }.toMap

  protected val _onNewState   = mutable.Buffer.empty[js.Function1[StateReport, Any]]
  protected val _onNewMessage = mutable.Buffer.empty[js.Function1[MessageReport, Any]]
}