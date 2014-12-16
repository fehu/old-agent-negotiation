package feh.tec.web

import feh.tec.web.QueenInfo.SelectionDisabled
import feh.tec.web.common.NQueenMessages
import feh.tec.web.common.NQueenMessages.{ChangeReport, MessageReport, Queen}
import org.scalajs.dom
import org.scalajs.jquery._

import scala.collection.mutable
import scala.scalajs.js
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
    def fallback(i: Int) = s"style-fallback-flag-$i"

    def generate(n: Int): String = generateAcceptance(n) :: generateTopPriority(n) :: Nil mkString "\n"

    def generateAcceptance(n: Int) =  generate(n, acceptance, i => s".queen-$i .name { color: green; }")
    def generateTopPriority(n: Int) = generate(n, fallback, i => s".queen-$i .name { color: orange; }")

    protected def generate(n: Int, id: Int => String, style: Int => String): String = (
      for (i <- 1 to n)
      yield tags2.style(
        *.`type`  := "text/css",
//        *.disabled  := "disabled",
        *.id      := id(i),
        raw(style(i))
      ).toString() + "\n" + tags.script(s"$$('#${id(i)}').prop('disabled', true)")
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

  def updatePositions(msg: NQueenMessages.CanBulk): Any = msg match {
    case NQueenMessages.ChangeReport(Queen(by), _, posOpt, _, _) =>
      posOpt.map(pos => updatePositions(Map(by -> pos)))
    case NQueenMessages.MessageReport(Queen(by), _, NQueenMessages.Message(_, _, content), _) =>
      updatePositions(Map(by -> content.position))
  }
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

@JSExport("feh.tec.web.ChessBoard")
object ChessBoard{

  trait JSInterface{
    self: ChessBoard =>

    @JSExport
    def bindHtml(to: String): Any
    @JSExport
    def bindHtml(): Any

    @JSExport
    def setWaiting(queen: Int): Any
    @JSExport
    def setWaiting(queens: js.Array[Int]): Any
    @JSExport
    def setFallback(queen: Int): Any
    @JSExport
    def setFallback(queens: js.Array[Int]): Any
    @JSExport
    def resetState(queen: Int): Any
    @JSExport
    def resetState(queens: js.Array[Int]): Any

    @JSExport
    def updatePositions(queen: Int, x: Int, y: Int): Any
    @JSExport
    def setAllPositions(fixed: String, positions: js.Array[Int]): Any
  }

  @JSExport
  def offline(size: Int): JSInterface = new ChessBoard(size) with JSInterface{

    def bindHtml(): Any = bindHtml("[containerfor=chess-board]")

    def bindHtml(to: String) = {
      jQuery("head") append QueenFlagStyles.generate(size)
      jQuery(to) html this.create
    }

    def setFallback(queens: js.Array[Int]) = for((queen: Int) <- queens) yield {
      jQuery("#" + QueenFlagStyles.fallback(queen)).prop("disabled", false)
      jQuery("#" + QueenFlagStyles.acceptance(queen)).prop("disabled", true)
    }

    def setWaiting(queens: js.Array[Int]) = for((queen: Int) <- queens) yield{
      jQuery("#" + QueenFlagStyles.acceptance(queen)).prop("disabled", false)
      jQuery("#" + QueenFlagStyles.fallback(queen)).prop("disabled", true)

    }

    def resetState(queens: js.Array[Int]) = for((queen: Int) <- queens) yield{
      jQuery("#" + QueenFlagStyles.acceptance(queen)).prop("disabled", true)
      jQuery("#" + QueenFlagStyles.fallback(queen)).prop("disabled", true)

    }

    def setWaiting(queen: Int) = setWaiting(js.Array(queen))
    def setFallback(queen: Int) = setFallback(js.Array(queen))
    def resetState(queen: Int) = resetState(js.Array(queen))

    def updatePositions(queen: Int, x: Int, y: Int) = this.updatePositions(Map(queen -> (x, y)))

    def setAllPositions(fixed: String, positions: js.Array[Int]): Any = this.updatePositions(
        (1 to size).map(i => i -> (fixed.trim.toLowerCase match {
        case "x" => i -> positions(i-1)
        case "y" => positions(i-1) -> i
      })).toMap
    )
  }
}

object QueenInfo{
  class Selection(val updFunc: Selection => Any, var left: Option[Int] = None, var right: Option[Int] = None){
    def isDefined = left.isDefined && right.isDefined
    def queens(nameL: String, nameR: String) =
      if(isDefined) (Queen(left.get), nameL) -> (Queen(right.get), nameR)
      else sys.error("!isDefined")
  }

  case object SelectionDisabled extends Selection(null)
}

class QueenInfo(name: String, val n: Int, selection: QueenInfo.Selection){

  def updatePriority(rep: MessageReport): JQuery = updatePriority(rep.by, rep.msg.priority)
  def updatePriority(queen: Queen, priority: Int): JQuery = {
    sel.queenInfo(queen.n) children ".priority" children "dd" text priority.toString
  }

  def updateState(rep: ChangeReport) = rep match {
    case ChangeReport(queen@Queen(q), at, posOpt, stateOpt, priorityOpt) =>
      priorityOpt.foreach( updatePriority(queen, _) )
//      for (pos <- posOpt) sel.queenInfo(q) children ".position" children "dd" text pos.toString
      for (state <- stateOpt) yield {
        val waiting = state != "Waiting"
        val fallback = state != "FallbackState"
        jQuery("#" + QueenFlagStyles.acceptance(q)).prop("disabled", waiting)
        jQuery("#" + QueenFlagStyles.fallback(q)).prop("disabled", fallback)
      }
  }

  def updateSelection() = if(selection != SelectionDisabled)
    for(i <- 1 to n ){
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


  def setCallbacks() = if(selection != SelectionDisabled)  {
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
