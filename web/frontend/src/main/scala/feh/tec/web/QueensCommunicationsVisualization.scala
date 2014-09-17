package feh.tec.web

import Utils._
import NQueenTemplates._
import feh.tec.web.common.NQueenMessages._
import org.scalajs.dom.Element
import org.scalajs.jquery._
import scala.scalajs.js
import scala.scalajs.js.Dynamic
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExport
import scalatags.Text.all._

class QueensCommunications(archive: ReportArchive){

  def create =
  div(`class` := "queen-comm",
    table( `class` := "messages tablesorter",
      thead(
        tr(
          th(`class` := "l            filter-false group-false"),
          th(`class` := "proposal     filter-false group-word ",                                        "Proposal"),
          th(`class` := "time                      group-false",                                        "Time"    ),
          th(`class` := "priority                  group-false",                                        "Priority"),
          th(`class` := "position                  group-false",                                        "Position"),
          th(`class` := "type                      group-false", "data-placeholder".attr := "Select",   "Type"    ),
          th(`class` := "weighted                  group-false", "data-placeholder".attr := "Select",   "Weighted"),
          th(`class` := "r            filter-false group-false")
        )
      ),
      tbody(),
      bottomFooter
    )

  ).toString()

  protected def bottomFooter =
    tfoot(
      tr(
        th(`class` := "l"),
        th("colspan".attr := 6, `class` := "ts-pager form-horizontal",
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
    case MessageReport(_, _, Message(id, priority, content), time, extra) =>
      val (proposal, pos, tpe, clazz, isProp) = content match {
        case Proposal(pos) => (id, pos, "Proposal", " proposal-msg", true)
        case Response(prop, tpe) => (prop, "", tpe.toString, " tablesorter-childRow", false)
      }
      val row = tr(
            `class` := "message" + clazz, "to".attr := dir,
        td( `class` := "l",           raw(if(dir == "left") genArrow(dir) else "")),
        td( `class` := "proposal",    proposal),
        td( `class` := "time",        time),
        td( `class` := "priority",    priority),
        td( `class` := "position",    pos.toString),
        td( `class` := "type",        tpe),
        td( `class` := "weighted",    extractExtraWeighted(extra)),
        td( `class` := "r",           raw(if(dir == "right") genArrow(dir) else ""))
      )
      (row.toString(), proposal, isProp)
  }

  protected def extractExtraWeighted(extra: Option[MessageExtraReport]) = extra.collectFirst{
    case ReportWeight(weight) if weight.size == 1 =>
      span(weight.head._2*100 + "% " + { if(weight.head._1.get) "acceptance" else "rejection" }) :: Nil
    case ReportWeight(weight) if weight.size == 2 && weight.forall(_._2 == 0) =>
      unknownSpan :: Nil
    case ReportWeight(weight) =>
      val wm = weight.toMap
      wm.get(Some(true)).map { w => span(`class` := "acceptance", s"acceptance: ${w*100}%") }.getOrElse(raw("")) ::
      wm.get(Some(false)).map{ w => span(`class` := "rejection",  s"rejection:  ${w*100}%") }.getOrElse(raw("")) ::
      wm.get(None).map       { w => span(`class` := "rejection",  s"unknown:    ${w*100}%") }.getOrElse(raw("")) :: Nil
  }.getOrElse(unknownSpan :: Nil)

  private def unknownSpan = span(`class` := "unknown", "unknown")

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
        val dirOpt = PartialFunction.condOpt(report.by -> report.to){
          case (`left`, `right`)  => "right"
          case (`right`, `left`)  => "left"
        }

        dirOpt map {
          dir =>
            val (row, prop, isProp) = createMessageRow(report, dir)

            if (isProp) jQuery(".queen-comm .messages tbody") append row
            else {
              val target = jQuery(".queen-comm .messages tr.proposal-msg").has(s"""td.proposal:contains("$prop")""")
              jQuery(row).insertAfter(target).children("td").hide()

//              Dynamic.global.console.log(qres)
//              qres.hide()
            }
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

  protected def clearOnNewMessage() = updateMessage foreach archive.rmOnNewMessagesCallbackosdo
  protected var updateMessage: Option[js.Function1[Map[Int, List[MessageReport]], Any]] = None
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
        "sortList" -> js.Array(js.Array(2, 0)),
        "headers" -> js.Dictionary(
          "6" -> js.Dictionary(
            "sorter" -> false,
            "filter" -> true
          )
        ),
        "widthFixed" -> true,
        "widgets" -> js.Array("filter", "zebra", "group"),
        "widgetOptions" -> ((filterWidgetOptions ++ groupWidgetOptions).toJSDictionary)
      )}
      .tablesorterPager(js.Dictionary(
        "container" ->  jQuery(".ts-pager"),
        "cssGoto" -> ".pagenum",
        "output" -> "{startRow} - {endRow} / {filteredRows} ({totalRows})"
      ))

    js.eval(
      """  $('.tablesorter').delegate('.message .proposal', 'click' ,function(){
        |    $(this).closest('tr').nextUntil('tr:not(.tablesorter-childRow)').find('td').toggle();
        |    return false;
        |  });
      """.stripMargin)
  }

  def filterWidgetOptions = js.Dictionary(
    "uitheme" -> "jui",
    "filter_functions" -> filterFunctions
  )
  def groupWidgetOptions = js.Dictionary(
    "group_collapsible" -> true,
    "group_collapsed"   -> false,
    "group_count"       -> false,
    "filter_childRows"  -> true
//    "textSorter"        -> sorterFunctions
//    "group_saveGroups"  -> true,
//    "filter_childRows"  -> true
  )

  def sorterFunctions = js.Dictionary(
    "3" -> { (a: js.Number, b: js.Number, direction: js.Boolean, column: js.Number, table: Element) =>

    }
  )

  def filterFunctions =
    js.Dictionary(
      "5" -> true,
      "6" -> js.Dictionary(
        "acceptance"  -> (((e: js.String, n: js.Any, f: js.String, i: js.Number) => e contains "acceptance" ): js.Function),
        "rejection"   -> (((e: js.String, n: js.Any, f: js.String, i: js.Number) => e contains "rejection" ): js.Function),
        "unknown"     -> (((e: js.String, n: js.Any, f: js.String, i: js.Number) => e contains "unknown" ): js.Function)
      )
    )
}