package feh.tec.web

import feh.tec.web.common.NQueenMessages.{Message, MessageReport, StateReport, Queen}
import org.scalajs.jquery._
import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport
import scalatags.Text.all._
import Utils._

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


  def updatePositions(state: StateReport): Any = updatePositions(Map(state.by.n -> state.position))
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
    case StateReport(Queen(q), pos, pr, _, _) =>
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
//    println("upd!!!")
    def upd(p: (Queen, String), s: String) = {
      val ss = jQuery(".queen-comm .messages th" + s) //sel.communications children ".messages" children ("th" + s) children ".name"
//      global.console.log(ss.get())
      ss text ( p._2 + "-" + p._1.n )
    }

    upd(left, ".l")
    upd(right, ".r")
    clearMessages()
    updateMessages(left._1, right._1)
  }

  protected def clearMessages() = jQuery(".queen-comm .message") remove()

  protected def createMessageRow(report: MessageReport, dir: String) = report match {
    case MessageReport(_, _, Message(priority, position, tpe), time) =>
      s"""<tr class="message" to="$dir">
         |  <td class="l">${ if(dir == "left") genArrow(dir) else "" }</td>
         |  <td class="time">$time</td>
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

  protected def appendMessages(reports: Map[Int, List[MessageReport]], left: Queen, right: Queen): Any =
    reports.flatMap(_._2) map{
      report =>
        val dir = PartialFunction.condOpt(report.by -> report.to){
          case (`left`, `right`)  => "right"
          case (`right`, `left`)  => "left"
        }
        dir map {
          sel.communications children ".messages" children "tbody" append createMessageRow(report, _)
        }
    }

  protected def genUpdateMessageFunc(left: Queen, right: Queen) = appendMessages(_: Map[Int, List[MessageReport]], left, right)

  protected def updateMessages(left: Queen, right: Queen) ={
    clearOnNewMessage()
    val msgs = archive.messages(Set(left.n, right.n))
    updateMessage = Option(genUpdateMessageFunc(left, right))
    archive.onNewMessages(updateMessage.get)
    updateMessage.get(msgs.groupBy(_.by.n))
  }

  protected def clearOnNewMessage() = updateMessage foreach archive.rmOnNewMessagesCallback
  protected var updateMessage: Option[js.Function1[Map[Int, List[MessageReport]], Any]] = None

  def create =
    s"""<div class="queen-comm">
       |  <table class="messages tablesorter">
       |    <thead>
       |      <tr>
       |        <th class="l"/>
       |        <th class="time">millis of neg.</th>
       |        <th class="priority">priority</th>
       |        <th class="position">position</th>
       |        <th class="type">type</th>
       |        <th class="r"/>
       |      </tr>
       |    </thead>
       |    <tbody/>
       |    $bottomFooter
       |  </table>
       |</div>
     """.stripMargin

  protected def bottomFooter =
    tfoot(
      tr(
        th(),
        th("colspan".attr := 4,
          button(`class` := "btn first",  i(`class` := "icon-step-backward glyphicon glyphicon-step-backward")),
          button(`class` := "btn prev",   i(`class` := "icon-arrow-left glyphicon glyphicon-backward")),
          span(`class` := "pagedisplay"),
          button(`class` := "btn next",   i(`class` := "icon-arrow-right glyphicon glyphicon-forward")),
          button(`class` := "btn last",   i(`class` := "icon-step-forward glyphicon glyphicon-step-forward")),
          select(`class` := "pagesize input-mini", title := "Select page size",
            option("selected".attr := "selected", value := 10, 10),
            option(value := 20, 20),
            option(value := 30, 30),
            option(value := 50, 50)
          ),
          select(`class` := "pagenum input-mini", title := "Select page number")
        ),
        th()
      )
    )
}

class QueensCommunicationsTableSorter(archive: ReportArchive) extends QueensCommunications(archive){

  protected def updateTableSorter() = jQuery(".queen-comm .messages").trigger("update")

  override protected def updateMessages(left: Queen, right: Queen) =
    super.updateMessages(left, right) $ updateTableSorter()


  override protected def genUpdateMessageFunc(left: Queen, right: Queen) =
    super.genUpdateMessageFunc(left, right) andThen{ _ $ updateTableSorter() }

  override def create = super.create + "\n" +
    script("$().ready(function(){ feh.tec.web.QueensCommunicationsTableSorter().init() })").toString()

}

@JSExport("feh.tec.web.QueensCommunicationsTableSorter")
object QueensCommunicationsTableSorter{
  @JSExport
  def init(): Unit = {
    jQuery("table.messages").asInstanceOf[js.Dynamic]
      .tablesorter(js.Dictionary(
        "theme" -> "blue",
        //      "headerTemplate" -> "{content}", // {icon}
        "widgets" -> js.Array("filter", "zebra")
      ))
      .tablesorterPager(js.Dictionary(
        "container" ->  jQuery(".ts-pager"),
        "cssGoto" -> ".pagenum",
        "output" -> "{startRow} - {endRow} / {filteredRows} ({totalRows})"
      ))
  }
}