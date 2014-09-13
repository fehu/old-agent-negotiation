package feh.tec.web

import feh.tec.web.common.NQueenMessages.{StateReport, Queen}
import org.scalajs.jquery._
import scala.scalajs.js.Dynamic.global
import scala.collection.mutable

object NQueenTemplates{

  object sel{
    def chessBoard          = jQuery(".chess-board")
    def queenInfo(n: Int)   = jQuery(s".queen-info .queen-$n")
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
      println(l)
      println(newLabels)
      jQuery(s".chess-board tr:nth-child(${i+1}) td:nth-child(${j+1})") text l
    }
  }
}


class QueenInfo(name: String, val n: Int){
  protected var selectedLeft: Option[Int] = None
  protected var selectedRight: Option[Int] = None

  def updateState(state: StateReport) = state match {
    case StateReport(Queen(q), pos, pr, _) =>
      sel.queenInfo(q) children ".priority" children "dd" text pr.toString
      sel.queenInfo(q) children ".position" children "dd" text pos.toString
  }

  def updateSelection() = for(i <- 1 to n ){
    selectedLeft.filter(i ==)
      .map{ _ => jQuery(selLeft(i)).addClass("selected") }
      .getOrElse{ jQuery(selLeft(i)).removeClass("selected") }
    selectedRight.filter(i ==)
      .map{ _ => jQuery(selRight(i)).addClass("selected") }
      .getOrElse{ jQuery(selRight(i)).removeClass("selected") }
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

  def comm =
    s"""<div class="comm">
       |  <p class="l"> <p class="name"/> </p>
       |  <p class="r"> <p class="name"/> </p>
       |  <p class="messages"/>
       |</div>
       |
     """.stripMargin

  def setCallbacks() = {
    for(i <- 1 to n ) {
      jQuery(selLeft(i)).click{ (ev: JQueryEventObject) =>
        selectedLeft = Some(i)
        updateSelection()
      }
      jQuery(selRight(i)).click{ (ev: JQueryEventObject) =>
        selectedRight = Some(i)
        updateSelection()
      }
    }
  }
  private def selLeft(n: Int)   = sel.queenInfo(n) children ".msg-sel.l"
  private def selRight(n: Int)  = sel.queenInfo(n) children ".msg-sel.r"
}
