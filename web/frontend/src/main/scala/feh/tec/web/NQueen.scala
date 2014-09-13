package feh.tec.web

import feh.tec.web.common.NQueenMessages
import feh.tec.web.common.NQueenMessages.{MessageReport, Queen, StateReport}
import org.scalajs.dom.{MouseEvent, MessageEvent, WebSocket, document}
import org.scalajs.jquery.{JQueryEventObject, jQuery}
import scala.scalajs.js.Dynamic.global

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.{JSON, JSApp}

object NQueen extends JSApp with NQueenSocketListener{
  def host = "ws://localhost:8080"
  def main() = {}

  protected var chessBoard: ChessBoard = null
  protected var queensInfo: Map[Int, QueenInfo] = null
  protected var reportArchive: ReportArchive = null

  def initNegotiationInfo(queens: Map[Int, String]): Any = {
    reportArchive = new ReportArchive(queens.keySet)
    chessBoard = new ChessBoard(queens.size)
    queensInfo = queens.map{ case (i, name) => i -> new QueenInfo(name, i) }.toMap

    jQuery(document.body) append s"""<div class="queen-info">${queensInfo.map(_._2.infoList).mkString("\n")}</div>"""
    queensInfo.foreach(_._2.setCallbacks())

    jQuery(document.body) append chessBoard.create

    reportArchive.onNewState{ (s: StateReport) =>
      chessBoard.updatePositions(s)
      queensInfo.foreach(_._2.updateState(s))
    }
  }

  def stateReport(report: StateReport) = reportArchive.report(report)
}


trait NQueenSocketListener extends SocketConnections{

  def host: String

  protected lazy val sockets = new WebSocket(host) :: Nil

  def initNegotiationInfo(queens: Map[Int, String]): Any
  def stateReport(report: StateReport): Any

  def onMessage: PartialFunction[js.Any, Unit] = {
    case msg: js.prim.String =>
      val json = JSON.parse(msg)
      global.console.log(json)
      json.$t.asInstanceOf[String] match {
        case "StateReport" =>
          stateReport(StateReport(
            of = Queen(json.of.n.asInstanceOf[Int]),
            position = json.position.asInstanceOf[js.Array[Int]](0) -> json.position.asInstanceOf[js.Array[Int]](1),
            priority = json.priority.asInstanceOf[Int],
            proposalAcceptance = json.proposalAcceptance.asInstanceOf[js.Array[js.Array[_]]].map(
              (x: js.Array[_]) => Queen(x(0).asInstanceOf[js.Dynamic].n.asInstanceOf[Int]) -> x(1).asInstanceOf[Boolean]
            )
          ))
        case "Init" =>
          val queensInit = json.queens.asInstanceOf[js.Array[js.Array[_]]].map(
            (arr: js.Array[_]) =>
              arr(0).asInstanceOf[js.Dynamic].n.asInstanceOf[Int] -> arr(1).asInstanceOf[String]
          )
          initNegotiationInfo(queensInit.toMap)
      }
  }
}

class ReportArchive(queens: Set[Int]){
  def report(state: StateReport): Unit  = { _states(state.of.n) += state; _onNewState   foreach (_.apply(state))}
  def report(msg: MessageReport): Unit  = { _messages(msg.by.n) += msg;   _onNewMessage foreach (_.apply(msg))  }

  def states(i: Int)    = _states(i).toList
  def messages(i: Int)  = _messages(i).toList

  def onNewState(func: js.Function1[StateReport, Any]): Unit     = { _onNewState += func }
  def onNewMessage(func: js.Function1[MessageReport, Any]): Unit = { _onNewMessage += func }

  protected val _states   = queens.map{ q => q -> mutable.Buffer.empty[StateReport]   }.toMap
  protected val _messages = queens.map{ q => q -> mutable.Buffer.empty[MessageReport] }.toMap

  protected val _onNewState   = mutable.Buffer.empty[js.Function1[StateReport, Any]]
  protected val _onNewMessage = mutable.Buffer.empty[js.Function1[MessageReport, Any]]
}