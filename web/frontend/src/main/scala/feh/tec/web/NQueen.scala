package feh.tec.web

import feh.tec.web.NQueenTemplates.QueenFlagStyles
import feh.tec.web.common.NQueenMessages._
import org.scalajs.jquery.jQuery

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.{Dynamic, JSApp}

object NQueen extends NQueen

trait NQueen extends JSApp with NQueenSocketListener{
  lazy val wsUrl = jQuery("head meta[ws]").attr("ws")
  def main() = {}

  object sel{
    val containerForInfo  = jQuery("[containerFor=queen-info]")
    val containerForBoard = jQuery("[containerFor=chess-board]")
  }

  protected var chessBoard: ChessBoard = null
  protected var queensInfo: Map[Int, QueenInfo] = null
  protected var reportArchive: ReportArchiveInterface = null
  protected var communications: QueensCommunications = null
  protected var selection: QueenInfo.Selection = null

  def queensInfoCommunicationsInit(queens: Map[Int, String]) = {
    communications = new QueensCommunications(reportArchive)
    selection = new QueenInfo.Selection(
      s => (communications.update _).tupled apply s.queens(queens(s.left.get), queens(s.right.get))
    )
  }

  def queensInfoCommunicationsLateInit(): Any = {
    jQuery(sel.containerForInfo) append communications.create.toString()
  }

  def initNegotiationInfo(queens: Map[Int, String]): Any = {
    reportArchive = new ReportArchive(queens.keySet)
    chessBoard = new ChessBoard(queens.size)
    queensInfoCommunicationsInit(queens)
    queensInfo = queens.map{ case (i, name) => i -> new QueenInfo(name, i, selection) }.toMap

    sel.containerForInfo append s"""<div class="queen-info">${queensInfo.map(_._2.infoList).mkString("\n")}</div>"""

    queensInfo.foreach(_._2.setCallbacks())

    jQuery("head") append QueenFlagStyles.generate(queens.size)
    jQuery(sel.containerForBoard) append chessBoard.create
    queensInfoCommunicationsLateInit()

    def maxBySecure[A <: CanBulk, B](reps: Map[Int, List[A]])(f: A => Option[B]) = reps.mapValues{
      rep =>
        val filtered = rep.filter(r => f(r).isDefined)
        if(filtered.nonEmpty) Some(filtered.maxBy(_.at)) else None
    }

    reportArchive.onStateChange{ (reps: Map[Int, List[ChangeReport]]) =>
      val latestPosOpts = maxBySecure(reps)(_.position)
      val latestStateOpts = maxBySecure(reps)(_.state)

      latestPosOpts.values.flatten.foreach(chessBoard.updatePositions) //.flatMap{ case (n, repOpt) => repOpt.map(n -> _.position.get) }.toMap
      queensInfo.foreach{
        case (i, q) => latestStateOpts.get(i) foreach (_.withFilter(_.state.isDefined).foreach(q.updateState))
      }
    }
    reportArchive.onNewMessages{(msg: Map[Int, scala.List[MessageReport]]) => msg.map{
      case (n, reps) =>
        if(reps.nonEmpty) {
          val max = reps.maxBy(_.at)
          queensInfo(n).updatePriority(max)
        }
    }}
  }

  def bulkReport(report: BulkReport): Any = reportArchive.report(report)
}

trait ReportArchiveInterface{
  def report(msg: BulkReport)

  def onStateChange(byQueen: js.Function1[Map[Int, List[ChangeReport]], Any])
  def onNewMessages(byQueen: js.Function1[Map[Int, List[MessageReport]], Any])
  def rmOnNewMessagesCallback(func: js.Function1[Map[Int, List[MessageReport]], Any])

  def messages: Map[Int, List[MessageReport]]
  def messages(i: Int): List[MessageReport]
  def messages(i: Set[Int]): List[MessageReport]
}

class ReportArchive(queens: Set[Int]) extends ReportArchiveInterface{

  def report(msg: BulkReport): Unit = {
    val grouped_c =
      msg.messages.toList.withFilter(_.isInstanceOf[ChangeReport]).map(_.asInstanceOf[ChangeReport]).groupBy(_.by.n)

    val grouped_m =
      msg.messages.toList.withFilter(_.isInstanceOf[MessageReport]).map(_.asInstanceOf[MessageReport]).groupBy(_.by.n)

    grouped_m foreach{ case (i, reps) => _messages(i) ++= reps}

    _onNewMessages foreach (_(grouped_m))
    _onStateChange foreach (_(grouped_c))
  }
  
  def messages: Map[Int, List[MessageReport]]    = _messages.mapValues(_.toList.sortBy(_.at))
  def messages(i: Int): List[MessageReport]      = _messages(i).toList.sortBy(_.at)
  def messages(i: Set[Int]): List[MessageReport] = _messages.filterKeys(i.contains).values.flatten.toList.sortBy(_.at)

  def onStateChange(byQueen: js.Function1[Map[Int, List[ChangeReport]], Any]): Unit     = { _onStateChange += byQueen }
  def onNewMessages(byQueen: js.Function1[Map[Int, List[MessageReport]], Any]): Unit = { _onNewMessages += byQueen }
  def rmOnNewMessagesCallback(func: js.Function1[Map[Int, List[MessageReport]], Any]): Unit = { _onNewMessages -= func }

//  protected val _states   = queens.map{ q => q -> mutable.Set.empty[StateReport]   }.toMap
  protected val _messages = queens.map{ q => q -> mutable.Set.empty[MessageReport] }.toMap

  protected val _onStateChange = mutable.Buffer.empty[js.Function1[Map[Int, List[ChangeReport]], Any]]
  protected val _onNewMessages = mutable.Buffer.empty[js.Function1[Map[Int, List[MessageReport]], Any]]
}

class ReportArchiveLite extends ReportArchiveInterface{
  def report(msg: BulkReport): Unit = {
    val grouped_c =
      msg.messages.toList.withFilter(_.isInstanceOf[ChangeReport]).map(_.asInstanceOf[ChangeReport]).groupBy(_.by.n)

    val grouped_m =
      msg.messages.toList.withFilter(_.isInstanceOf[MessageReport]).map(_.asInstanceOf[MessageReport]).groupBy(_.by.n)

    _onNewMessages foreach (_(grouped_m))
    _onStateChange foreach (_(grouped_c))
  }

  def onStateChange(byQueen: js.Function1[Map[Int, List[ChangeReport]], Any]): Unit     = { _onStateChange += byQueen }
  def onNewMessages(byQueen: js.Function1[Map[Int, List[MessageReport]], Any]): Unit = { _onNewMessages += byQueen }
  def rmOnNewMessagesCallback(func: js.Function1[Map[Int, List[MessageReport]], Any]): Unit = { _onNewMessages -= func }

  protected val _onStateChange = mutable.Buffer.empty[js.Function1[Map[Int, List[ChangeReport]], Any]]
  protected val _onNewMessages = mutable.Buffer.empty[js.Function1[Map[Int, List[MessageReport]], Any]]

  def messages: Map[Int, List[MessageReport]] = ???
  def messages(i: Int): List[MessageReport] = ???
  def messages(i: Set[Int]): List[MessageReport] = ???
}