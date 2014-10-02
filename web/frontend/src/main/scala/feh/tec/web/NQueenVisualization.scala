package feh.tec.web

import feh.tec.web.common.NQueenMessages.{Queen, StateReport}
import scalajs.js
import org.scalajs.dom
import org.scalajs.jquery._
import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport
import scalatags.Text.short._
import scalatags.Text.{tags, tags2}


object NQueenTemplates{

  object sel{
    def chessBoard          = jQuery(".chess-board")
    def queenInfo(n: Int)   = jQuery(s".queen-info .queen-$n")
    def communications      = jQuery(".queen-comm")
  }

  object QueenFlagStyles{
    def acceptance(i: Int) = s"style-acceptance-flag-$i"
    def topPriority(i: Int) = s"style-top-priority-flag-$i"

    def generate(n: Int): String = generateAcceptance(n) :: generateTopPriority(n) :: Nil mkString "\n"

    def generateAcceptance(n: Int) =  generate(n, acceptance, i => s".queen-$i .name { color: green; }")
    def generateTopPriority(n: Int) = generate(n, topPriority, i => s".queen-$i .name { color: orange; }")

    protected def generate(n: Int, id: Int => String, style: Int => String): String = (
      for (i <- 1 to n)
      yield tags2.style(
        *.`type`  := "text/css",
        *.disabled  := true,
        *.id      := id(i),
        raw(style(i))
      )
    ).mkString("\n")
  }
}

class NQueenRestart{
  def negotiationFinishedAndWillRestart(delay: Int) = {
    NQueen.sel.containerForBoard append restartHtml(delay).toString()
  }
  def restart() = dom.window.location.reload()

  private def restartHtml(countdown: Int) = tags.span(
    *.`class` := "restart-negotiation",
    "The negotiation is finished!", tags.br, "It will restart approximately in ", tags.span(*.`class` := "countdown"), " seconds",
    tags.script(raw(s"NQueenRestart().countdown($countdown)"))
  )
}

@JSExport
object NQueenRestart{
  @JSExport
  def countdown(startingWith: js.Number): Unit ={
    jQuery(".restart-negotiation .countdown") text (startingWith / 1000).toInt.toString
    dom.window.setTimeout((i: Int) => countdown(i), 1000, startingWith-1000)
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

  protected def selectSquare(x: Int, y: Int) = jQuery(s".chess-board tr:nth-child(${y+1}) td:nth-child(${x+1})")

  def updatePositions(state: StateReport): Any = updatePositions(Map(state.by.n -> state.position))
  def updatePositions(posUpdate: Map[Int, (Int, Int)]): Any = {
    if (posUpdate.forall(upd => placedQueens.exists(upd ==))) return {}
    placedQueens ++= posUpdate.toSeq
    val newLabels = placedQueens.groupBy(_._2)
      .mapValues(_.keySet.map(i =>
        tags.span(*.`class` := s"queen-$i", tags.span(*.`class` := "name", i))
      ).mkString(",")).withDefaultValue("")
    for(i <- 1 to size; j <- 1 to size) {
      selectSquare(i, j) html newLabels.getOrElse(i -> j, "")
    }
  }

  def failedPositionClass = "glyphicon-remove"

  def positionProvenFailure(pos: (Int, Int)) = (selectSquare _).tupled(pos) addClass failedPositionClass
  def resetFailedPositions() = jQuery(".chess-board td." + failedPositionClass) removeClass failedPositionClass
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
    case StateReport(Queen(q), pos, pr, _, _, acceptFlag, topPriorityFlag) =>
      sel.queenInfo(q) children ".priority" children "dd" text pr.toString
      sel.queenInfo(q) children ".position" children "dd" text pos.toString

      jQuery("#" + QueenFlagStyles.acceptance(q)).prop("disabled", !acceptFlag || topPriorityFlag)
      jQuery("#" + QueenFlagStyles.topPriority(q)).prop("disabled", !topPriorityFlag)
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
