package feh.tec.web

import feh.tec.web.common.NQueenMessages.{Message, MessageReport, StateReport, Queen}
import org.scalajs.jquery._
import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.collection.mutable

object NQueenTemplates{

  object sel{
    def chessBoard          = jQuery(".chess-board")
    def queenInfo(n: Int)   = jQuery(s".queen-info .queen-$n")
    def communications      = jQuery(".queen-comm")
  }

}

import NQueenTemplates._

class ChessBoard(size: Int) {
  def create = s"""
      <table class="chess-board">
        ${(0 to size).map{
    i => s"""<tr>${ (0 to size).map{
      case 0 if i == 0  => "<td class=\"description\"></td>"
      case j if i == 0  => s"""<td class="description">$j</td>"""
      case 0            => s"""<td class="description">$i</td>"""
      case _            => "<td></td>"
    }.mkString("") }</tr>"""
  }.mkString("\n")}
      </table>"""

  protected val placedQueens = mutable.HashMap.empty[Int, (Int, Int)] // queen's number -> position


  def updatePositions(state: StateReport): Any = updatePositions(Map(state.of.n -> state.position))
  def updatePositions(posUpdate: Map[Int, (Int, Int)]): Any = {
    if (posUpdate.forall(upd => placedQueens.exists(upd ==))) return {}
    placedQueens ++= posUpdate.toSeq
    val newLabels = placedQueens.groupBy(_._2).mapValues(_.keySet).mapValues(_.mkString(",")).withDefaultValue("")
    for(i <- 1 to size; j <- 1 to size) {
      val l = newLabels.getOrElse(i -> j, "")
      jQuery(s".chess-board tr:nth-child(${i+1}) td:nth-child(${j+1})") text l
    }
  }
}

object QueenInfo{
  class Selection(val updFunc: Selection => Any, var left: Option[Int] = None, var right: Option[Int] = None){
    def isDefined = left.isDefined && right.isDefined
    def queens(nameL: String, nameR: String) =
      if(isDefined) (Queen(left.get), nameL) -> (Queen(right.get), nameR)
      else sys.error("!isDefined")
  }
}

class QueenInfo(name: String, val n: Int, selection: QueenInfo.Selection){

  def updateState(state: StateReport) = state match {
    case StateReport(Queen(q), pos, pr, _) =>
      sel.queenInfo(q) children ".priority" children "dd" text pr.toString
      sel.queenInfo(q) children ".position" children "dd" text pos.toString
  }

  def updateSelection() = for(i <- 1 to n ){
    selection.left.filter(i ==)
      .map{ _ => jQuery(selLeft(i)).addClass("selected") }
      .getOrElse{ jQuery(selLeft(i)).removeClass("selected") }
    selection.right.filter(i ==)
      .map{ _ => jQuery(selRight(i)).addClass("selected") }
      .getOrElse{ jQuery(selRight(i)).removeClass("selected") }
    if(selection.isDefined) selection.updFunc(selection)
  }

  def infoList =
    s"""<div class="queen-$n">
         |  <p class="msg-sel l">▶</p>
         |  <p class="msg-sel r">◀</p>
         |  <p class="name">$name-<b>$n</b></p>
         |  <dl class="priority"> <dt>Priority<dt>  <dd/> </dl>
         |  <dl class="position"> <dt>Position</dt> <dd/> </dl>
         |  <p class="button">History</p>
         |</div>
       """.stripMargin


  def setCallbacks() = {
    for(i <- 1 to n ) {
      jQuery(selLeft(i)).click{ (ev: JQueryEventObject) =>
        selection.left = Some(i)
        updateSelection()
      }
      jQuery(selRight(i)).click{ (ev: JQueryEventObject) =>
        selection.right = Some(i)
        updateSelection()
      }
    }
  }
  private def selLeft(n: Int)   = sel.queenInfo(n) children ".msg-sel.l"
  private def selRight(n: Int)  = sel.queenInfo(n) children ".msg-sel.r"

}


class QueensCommunications(archive: ReportArchive){

  def update(left: (Queen, String), right: (Queen, String)) = {
    println("upd!!!")
    def upd(p: (Queen, String), s: String) = {
      val ss = jQuery(".queen-comm .messages th" + s) //sel.communications children ".messages" children ("th" + s) children ".name"
      global.console.log(ss.get())
      ss text ( p._2 + "-" + p._1.n )
    }

    upd(left, ".l")
    upd(right, ".r")
    clearMessages()
    updateMessages(left._1, right._1)
  }

  protected def clearMessages() = jQuery(".queen-comm .message") remove()

  protected def createMessageRow(report: MessageReport, dir: String) = report match {
    case MessageReport(_, _, Message(priority, position, tpe)) =>
      s"""<tr class="message" to="$dir">
         |  <td class="l">${ if(dir == "left") genArrow(dir) else "" }</td>
         |  <td class="priority">$priority</td>
         |  <td class="position">$position</td>
         |  <td class="type">$tpe</td>
         |  <td class="r">${ if(dir == "right") genArrow(dir) else "" }</td>
         |</tr>
       """.stripMargin
  }

  protected def genArrow(dir: String) = {
    s"""<svg width="75px" height="16px" version="1.1" xmlns="http://www.w3.org/2000/svg">
       |  <rect x="5" y="6" width="65" height="4" stroke="black" fill="black" stroke-width="1"/>
       |  ${dir match{
              case "left"   => """<path d="M  0 8, L  0 7, L 10 0, V 16, Z"/>"""
              case "right"  => """<path d="M 75 8, L 75 7, L 65 0, V 16, Z"/>"""
            }}
       |</svg>
     """.stripMargin
  }

  protected def appendMessage(report: MessageReport, left: Queen, right: Queen): Any ={
    val dir = PartialFunction.condOpt(report.by -> report.to){
      case (`left`, `right`)  => "right"
      case (`right`, `left`)  => "left"
    }
    dir map {
      sel.communications children ".messages" append createMessageRow(report, _)
    }

  }


  protected def updateMessages(left: Queen, right: Queen): Any ={
//    println("updateMessages")
    clearOnNewMessage()
    val msgs = archive.messages(Set(left.n, right.n))
    updateMessage = Option(appendMessage(_: MessageReport, left, right))
    archive.onNewMessage(updateMessage.get)
    msgs foreach updateMessage.get
  }

  protected def clearOnNewMessage() = updateMessage foreach archive.rmOnNewMessage
  protected var updateMessage: Option[js.Function1[MessageReport, Any]] = None

  def create =
    s"""<div class="queen-comm">
       |  <table class="messages">
       |    <tr>
       |      <th class="l"/>
       |      <th class="priority">priority</th>
       |      <th class="position">position</th>
       |      <th class="type">type</th>
       |      <th class="r"/>
       |    </tr>
       |  </table>
       |</div>
     """.stripMargin
}