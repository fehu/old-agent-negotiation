package feh.tec.web

import feh.tec.web.common.NQueenMessages.{Queen, StateReport}
import org.scalajs.jquery._
import scala.collection.mutable
import scalatags.Text.short._
import scalatags.Text.{tags, tags2}


object NQueenTemplates{

  object sel{
    def chessBoard          = jQuery(".chess-board")
    def queenInfo(n: Int)   = jQuery(s".queen-info .queen-$n")
    def communications      = jQuery(".queen-comm")
  }

  object QueenAcceptanceFlagStyles{
    def id(i: Int) = s"style-acceptance-flag-$i"

    def generate(n: Int) = (
      for (i <- 1 to n)
        yield tags2.style(
          *.`type`  := "text/css",
          *.disabled  := true,
          *.id      := id(i),
          raw(
            s"""
              |.queen-$i .name { color: green; }
            """.stripMargin)
        )
      ).mkString("\n")
  }
}

import feh.tec.web.NQueenTemplates._

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


  def updatePositions(state: StateReport): Any = updatePositions(Map(state.by.n -> state.position))
  def updatePositions(posUpdate: Map[Int, (Int, Int)]): Any = {
    if (posUpdate.forall(upd => placedQueens.exists(upd ==))) return {}
    placedQueens ++= posUpdate.toSeq
    val newLabels = placedQueens.groupBy(_._2)
      .mapValues(_.keySet.map(i =>
        tags.span(*.`class` := s"queen-$i", tags.span(*.`class` := "name", i))
      ).mkString(",")).withDefaultValue("")
    for(i <- 1 to size; j <- 1 to size) {
      jQuery(s".chess-board tr:nth-child(${i+1}) td:nth-child(${j+1})") html newLabels.getOrElse(i -> j, "")
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
    case StateReport(Queen(q), pos, pr, _, _, acceptFlag) =>
      sel.queenInfo(q) children ".priority" children "dd" text pr.toString
      sel.queenInfo(q) children ".position" children "dd" text pos.toString

      jQuery("#" + QueenAcceptanceFlagStyles.id(q)).prop("disabled", !acceptFlag)
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
