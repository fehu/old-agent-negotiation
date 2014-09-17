package feh.tec.web

import Utils._
import NQueenTemplates._
import feh.tec.web.common.NQueenMessages._
import org.scalajs.jquery._
import scala.scalajs.js
import scala.scalajs.js.Dynamic
import scala.scalajs.js.annotation.JSExport
import scalatags.Text.all._

class QueensCommunications(archive: ReportArchive){

  def update(left: (Queen, String), right: (Queen, String)) = {
    //    println("upd!!!")
    def upd(p: (Queen, String), s: String) = {
      val ss = jQuery(".queen-comm .messages thead th" + s)
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
    case MessageReport(_, _, Message(priority, position, tpe), time, extra) =>
      s"""<tr class="message" to="$dir">
         |  <td class="l">${ if(dir == "left") genArrow(dir) else "" }</td>
         |  <td class="time">$time</td>
         |  <td class="priority">$priority</td>
         |  <td class="position">$position</td>
         |  <td class="type">$tpe</td>
         |  <td class="weighted">${extractExtraWeighted(extra)}</td>
         |  <td class="r">${ if(dir == "right") genArrow(dir) else "" }</td>
         |</tr>
       """.stripMargin
  }

  protected def extractExtraWeighted(extra: Option[MessageExtraReport]) = extra.collectFirst{
    case ReportWeight(weight) if weight.size == 1 =>
      weight.head._2*100 + "% " + { if(weight.head._1.get) "acceptance" else "rejection" }
    case ReportWeight(weight) if weight.size == 2 && weight.forall(_._2 == 0) =>
      unknownSpan
    case ReportWeight(weight) =>
      val wm = weight.toMap
      span(`class` := "acceptance", s"acceptance: ${wm(Some(true))  *100}%").toString() +
      span(`class` := "rejection",  s"rejection:  ${wm(Some(false)) *100}%").toString()
  }.getOrElse(unknownSpan)

  private def unknownSpan = span(`class` := "unknown", "unknown").toString()

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
       |        <th class="l filter-false"/>
       |        <th class="time">millis of neg.</th>
       |        <th class="priority">priority</th>
       |        <th class="position">position</th>
       |        <th class="type" data-placeholder="Select">type</th>
       |        <th class="weighted" data-placeholder="Select">weighted</th>
       |        <th class="r filter-false"/>
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
        th(`class` := "l"),
        th("colspan".attr := 5, `class` := "ts-pager form-horizontal",
          button(`class` := "btn first", `type` := "button",
            i(`class` := "icon-step-backward glyphicon glyphicon-step-backward")),
          button(`class` := "btn prev", `type` := "button",
            i(`class` := "icon-arrow-left glyphicon glyphicon-backward")),
          span(`class` := "pagedisplay"),
          button(`class` := "btn next", `type` := "button",
            i(`class` := "icon-arrow-right glyphicon glyphicon-forward")),
          button(`class` := "btn last", `type` := "button",
            i(`class` := "icon-step-forward glyphicon glyphicon-step-forward")),
          select(`class` := "pagesize input-mini", title := "Select page size",
            option("selected".attr := "selected", value := 10, 10),
            option(value := 20, 20),
            option(value := 30, 30),
            option(value := 50, 50)
          ),
          select(`class` := "pagenum input-mini", title := "Select page number")
        ),
        th(`class` := "r")
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
      .tablesorter{js.Dictionary(
        "theme" -> "jui",
        "headers" -> js.Dictionary(
          "5" -> js.Dictionary(
            "sorter" -> false,
            "filter" -> true
          )
        ),
        "widthFixed" -> true,
        "widgets" -> js.Array("filter", "zebra"),
        "sortList" -> js.Array(js.Array(1, 0)),
        "widgetOptions" -> js.Dictionary(
          "uitheme" -> "jui",
          "filter_functions" -> filterFunctions
        )
      )}
      .tablesorterPager(js.Dictionary(
        "container" ->  jQuery(".ts-pager"),
        "cssGoto" -> ".pagenum",
        "output" -> "{startRow} - {endRow} / {filteredRows} ({totalRows})"
      ))
  }

  def filterFunctions =
    js.Dictionary(
      "4" -> true,
      "5" -> js.Dictionary(
        "acceptance"  -> (((e: js.String, n: js.Any, f: js.String, i: js.Number) => e contains "acceptance" ): js.Function),
        "rejection"   -> (((e: js.String, n: js.Any, f: js.String, i: js.Number) => e contains "rejection" ): js.Function),
        "unknown"     -> (((e: js.String, n: js.Any, f: js.String, i: js.Number) => e contains "unknown" ): js.Function)
      )
    )
}