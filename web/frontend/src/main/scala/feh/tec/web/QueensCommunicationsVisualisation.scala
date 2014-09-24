package feh.tec.web

import feh.tec.web.common.NQueenMessages._
import org.scalajs.dom.Element
import org.scalajs.jquery._

import scala.collection.mutable
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSConverters._
import scala.scalajs.js
import scalatags.Text
import scalatags.Text.{ all => tg }
import tg._

trait QueensCommunicationsApi{
  def update(left: (Queen, String), right: (Queen, String))

  def cacheNewMessages(reports: Map[Int, List[MessageReport]]): Any
  
  def create: Text.TypedTag[_]
}

class QueensCommunications(archive: ReportArchive) extends QueensCommunicationsApi with QueensCommunicationsTemplates{
  cacheNewMessages(archive.messages.mapValues(_.toList))
  archive.onNewMessages{(msg: Map[Int, scala.List[MessageReport]]) => cacheNewMessages(msg); showNewMessages(msg)}

  QueensCommunications.archive = archive
  QueensCommunications.comm = this

  protected[QueensCommunications] object cache{
    val proposals = mutable.Map.empty[QueensCommunications.ProposalId, ReportRow]
    val responses = mutable.Map.empty[String, mutable.Buffer[ReportRow]] // grouped by proposal
  }

  protected var leftQueen: Queen = null
  protected var rightQueen: Queen = null

  protected def selBody = ".queen-comm .messages tbody"

  def update(left: (Queen, String), right: (Queen, String)) = {
    def upd(p: (Queen, String), s: String) = jQuery(".queen-comm .messages thead th" + s) text ( p._2 + "-" + p._1.n )

    leftQueen = left._1
    rightQueen = right._1

    upd(left, ".l")
    upd(right, ".r")
    clearMessages()
    showMessages()
  }

  def showMessages(): Unit ={
    cache.proposals.filter(filterMessages compose (_._2)).map{
      case(propId, propRow) =>
        val html =
          (propRow +: cache.responses.getOrElse(propId.id, Nil)) withFilter filterMessages map (setDir _ andThen (_.toString()))
        if(html.nonEmpty) html.foreach(jQuery(selBody).append(_: String))
//        jQuery(selBody + " tr.proposal-msg:visible").asInstanceOf[js.Dynamic].toggle()
    }
    updateTableSorter()
  }

  def showNewMessages(messages: Map[Int, scala.List[MessageReport]]): Unit = if(leftQueen != null && rightQueen != null){
    val mLeft   = messages.getOrElse(leftQueen.n, Nil).filter(_.to == rightQueen)
    val mRight = messages.getOrElse(rightQueen.n, Nil).filter(_.to == leftQueen)

    val proposals = (mLeft ::: mRight) collect {
      case prop if prop.msg.isProposal => cache.proposals(QueensCommunications.ProposalId(prop.id, prop.to, prop.at))
    }
    val responses = (mLeft ::: mRight) collect {
      case MessageReport(_, _, Message(id, _, Response(prop, _)), _, _) => prop -> cache.responses(prop).find(_.id == id).get
    } groupBy (_._1)

    jQuery(".queen-comm .messages tbody") append proposals.map(setDir).mkString

    if(responses.isEmpty) return

    responses foreach{
      case (prop, resps) => jQuery(s"tr.proposal-msg[proposal=$prop]") after resps.map(this setDir _._2).mkString
    }
    updateTableSorter()
  }

  protected def filterMessages: ReportRow => Boolean = row => PartialFunction.cond(row.from -> row.to)(withLRQ(true, true))
//    row.from == leftQueen && row.to == rightQueen || row.to == leftQueen && row.from == rightQueen

  protected def setDir(row: ReportRow) = {
    val dir = withLRQ("left", "right")(row.from -> row.to)
    row.tag("to".attr := dir)
  }

  private def withLRQ[R](caseLeft: R, caseRight: R): PartialFunction[(Queen, Queen), R] = {
    case (lq, rq) if lq == leftQueen && rq == rightQueen  => caseLeft
    case (rq, lq) if lq == leftQueen && rq == rightQueen  => caseRight
  }

  case class ReportRow(tag: Text.Tag, from: Queen, to: Queen, id: String, isProposal: Boolean)

  protected def clearMessages() = jQuery(".queen-comm .message") remove()

  def cacheNewMessages(reportsMap: Map[Int, List[MessageReport]]) = reportsMap foreach{
    case (sentBy, reports) => reports foreach{
      case rep@MessageReport(_, to, Message(prop, _, Proposal(_)), at, _)     =>
        cache.proposals += QueensCommunications.ProposalId(prop, to, at) -> row(rep)
      case rep@MessageReport(_, _, Message(_, _, Response(prop, _)), _, _)  =>
        cache.responses.getOrElseUpdate(prop, mutable.Buffer.empty) += row(rep)
    }
  }

  protected def updateTableSorter() = jQuery(".queen-comm .messages").trigger("update")

  override def create = super.create apply
    script(s"$$().ready(function(){ QueensCommunicationsSorter().init() })")
}

@JSExport
object QueensCommunications{

  var archive: ReportArchive = null
  var comm: QueensCommunications = null

  case class ProposalId(id: String, to: Queen, at: Int)

  @JSExport
  def diffArchiveAndCache(queens: js.Array[Int]): Unit = diffArchiveAndCache(queens.toList)
  def diffArchiveAndCache(queens: List[Int]): Unit = queens match {
    case h :: t => 
      for(i <- t) println(diffArchiveAndCacheBetween(h, i))
      diffArchiveAndCache(t)
    case Nil =>
  }

  private def fromArchive(q1: Int, q2: Int) = archive.messages.values.flatten.filter(
    rep => rep.reportsMessage && ( rep.by.n == q1 && rep.to.n == q2 || rep.by.n == q2 && rep.to.n == q1)
  )

  @JSExport
  def diffArchiveAndCacheBetween(q1: Int, q2: Int) = {
    val ca = fromArchive(q1, q2)
    val cap = ca.count(_.msg.isProposal)
    val msgs = comm.cache.proposals.values ++ comm.cache.responses.values.flatten
    val cc = msgs.filter(row => row.from.n == q1 && row.to.n == q2 || row.from.n == q2 && row.to.n == q1)
    val ccp = cc.count(_.isProposal)

    if(ca.size == cc.size && cap == ccp) s"cache and archive match for Queen($q1) and Queen($q2)"
    else
      s"${ca.size} messages in archive, proposals: $cap\n${cc.size} messages in cache, proposals: $ccp\n"
  }

  @JSExport
  def diffArchiveAndTheTable(q1: Int, q2: Int) = {
    fromArchive(q1, q2) foreach {
      case MessageReport(_, _, Message(prop, _, Proposal(_)), _, _) =>
        if(jQuery(s".tablesorter tr.proposal-msg[proposal=$prop]").asInstanceOf[js.Dynamic].size().asInstanceOf[Int] == 0)
          println(s"proposal $prop has no representation in the table")
      case MessageReport(_, _, Message(repId, _, Response(prop, _)), _, _) =>
        if(jQuery(s".tablesorter tr.tablesorter-childRow[reportId=$repId]").asInstanceOf[js.Dynamic].size().asInstanceOf[Int] == 0)
          println(s"response $repId to proposal $prop has no representation in the table")
    }

  }
}

trait QueensCommunicationsTemplates{
  self: QueensCommunications =>

  def create = div(`class` := "queen-comm", table)
  def table = tg.table( `class` := "messages tablesorter", header, tbody(), footer)
  def header = thead(
    tr(
      th(`class` := "l            filter-false group-false"),
      th(`class` := "time                      group-false",                                        "Time"    ),
      th(`class` := "priority                  group-false",                                        "Priority"),
      th(`class` := "position                  group-false",                                        "Position"),
      th(`class` := "type                      group-false", "data-placeholder".attr := "Select",   "Type"    ),
      th(`class` := "weighted                  group-false", "data-placeholder".attr := "Select",   "Weighted"),
      th(`class` := "r            filter-false group-false")
    )
  )
  def footer =
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
  def row(report: MessageReport): ReportRow = {
    val (proposal, pos, tpe, clazz, isProp) = report.msg.content match {
      case Proposal(pos)        => (report.id, pos, "Proposal", "proposal-msg", true)
      case Response(prop, tpe)  => (prop, "", tpe.toString,     "tablesorter-childRow", false)
    }
    val html =
      tr(
        `class` := "message " + clazz,
        if(isProp) "proposal".attr := proposal else "reportId".attr := report.id,

        td( `class` := "l"            ),
        td( `class` := "time",        report.at),
        td( `class` := "priority",    report.msg.priority),
        td( `class` := "position",    pos.toString),
        td( `class` := "type",        tpe),
        td( `class` := "weighted",    extractExtraWeighted(report.extra)),
        td( `class` := "r")
      )
    ReportRow(html, report.by, report.to, report.id, isProp)
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

}

@JSExport
object QueensCommunicationsSorter{
  @JSExport
  def init(): Unit = {
    jQuery("table.messages").asInstanceOf[js.Dynamic]
      .tablesorter{js.Dictionary(
      "theme" -> "jui",
      "sortList" -> js.Array(js.Array(1, 0)),
      "headers" -> js.Dictionary(
        "5" -> js.Dictionary(
          "sorter" -> false,
          "filter" -> true
        )
      ),
      "widthFixed" -> true,
      "widgets" -> js.Array("filter"/*, "zebra"*/, "group"),
      "widgetOptions" -> (filterWidgetOptions ++ groupWidgetOptions).toJSDictionary
    )}
      .tablesorterPager(js.Dictionary(
      "container" ->  jQuery(".ts-pager"),
      "cssGoto" -> ".pagenum",
      "output" -> "{startRow} - {endRow} / {filteredRows} ({totalRows})"
    ))

    js.eval(
      """  $('.tablesorter').delegate('tr.proposal-msg', 'click' ,function(){
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
      "4" -> true,
      "5" -> js.Dictionary(
        "acceptance"  -> (((e: js.String, n: js.Any, f: js.String, i: js.Number) => e contains "acceptance" ): js.Function),
        "rejection"   -> (((e: js.String, n: js.Any, f: js.String, i: js.Number) => e contains "rejection" ): js.Function),
        "unknown"     -> (((e: js.String, n: js.Any, f: js.String, i: js.Number) => e contains "unknown" ): js.Function)
      )
    )
}