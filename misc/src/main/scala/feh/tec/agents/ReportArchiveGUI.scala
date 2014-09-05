package feh.tec.agents

import java.awt.{Toolkit, Dimension}
import java.util.Date

import feh.dsl.swing.SwingAppBuildingEnvironment
import feh.tec.agents.Message.AutoId
import feh.tec.agents.ReportArchiveGUI.ShowReports
import feh.tec.agents.impl.AgentReports.{StateReportEntry, MessageReport, StateReport}
import feh.tec.agents.impl.{AgentReport, ReportArchive}
import feh.util._
import scala.collection.mutable

// todo
class ReportArchiveGUI extends ReportArchive with SwingAppBuildingEnvironment{

  lazy val states   = mutable.HashMap.empty[AgentRef, mutable.Buffer[StateReport]] withDefault(_ => mutable.Buffer.empty)
  lazy val messages = mutable.HashMap.empty[AgentRef, mutable.Buffer[MessageReport]] withDefault(_ => mutable.Buffer.empty)

  def reports = (states ++ messages).toMap // .mapValues(_.toSeq)

  def newReport(rep: AgentReport) = {
    rep match{
      case rep: StateReport => states <<= (rep.of, _ += rep)
      case rep: MessageReport => messages <<= (rep.of, _ += rep)
    }
    log.info(s"[ReportArchiveGUI] guis=$guis")
    guis.get(rep.of).foreach{
      gui =>
        gui.updateForms()
        gui.logUpdatable()
    }
  }

  val guis = mutable.HashMap.empty[AgentRef, ReportGui]

  override def processSys = super.processSys orElse{
    case ShowReports(ag) =>
      val size = Toolkit.getDefaultToolkit.getScreenSize
      val gui = guis.getOrElse(ag, new ReportGui(ag, size) $$ { guis += ag -> _ })
      log.info(s"ShowReports($ag), gui=$gui, guis=$guis")
      gui.start()
      gui.updateForms()
  }

  def createReportWindow(of: AgentRef, size: Dimension): ReportGui = new ReportGui(of, size)

  class ReportGui(ag: AgentRef, _size: Dimension) extends SwingAppFrame with Frame9PositionsLayoutBuilder{

    def logUpdatable() = log.info("[ReportGui] updatable: " + componentAccess.updatable)

    private def cut(a: Any, b: Int, e: Int) = a.toString.drop(b).dropRight(e)

    def getStates = {
      val s = states(ag).toSeq.flatMap{
        case StateReport(of, rep, _) => rep map{
          case (neg, StateReportEntry(p, v, s, e)) =>
            (neg.name, p.get, cut(v, 4, 1), cut(s.map(_.id), 4, 1), e.map(_.toString).getOrElse(""))
        }
      }

      log.info(s"[ReportGui]: states of $ag: $s")
      s
    }

    def getMessages = {
      messages(ag).map{
        case MessageReport(to, msg) => (cut(to.id, 3, 1), msg)
      }
    }

    lazy val guiStates = monitorForSeq(getStates).table(
      "negotiation" -> classOf[String],
      "priority" -> classOf[Int],
      "values" -> classOf[String],
      "scope" -> classOf[String],
      "extra" -> classOf[String]
    )

    lazy val guiMessages = monitorForSeq(getMessages).table(
      "to" -> classOf[String],
      "msg" -> classOf[Message]
    )

    lazy val reports = panel.grid(1, 1)(
      split(_.Vertical)(
        scrollable()(guiStates, "states").withoutId,
        scrollable()(guiMessages, "messages").withoutId
      ).withoutId
    )

    lazy val layout: List[AbstractLayoutSetting] =  List(
      place(reports.fillBoth.maxXWeight.maxYWeight, "reports") in theCenter,
      place(label(ag.toString), noId) to theNorth of "reports"
    )


    def start() = {
      open()
      size = _size
      preferredSize = _size
      buildLayout()
    }
    def stop() = close()
  }
}


object ReportArchiveGUI{
  case class ShowReports(of: AgentRef) extends SystemMessage with AutoId
}