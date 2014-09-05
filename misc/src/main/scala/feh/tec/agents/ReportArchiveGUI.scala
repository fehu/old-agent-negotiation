package feh.tec.agents

import java.awt.{Toolkit, Dimension}
import java.util.Date

import feh.dsl.swing.SwingAppBuildingEnvironment
import feh.tec.agents.Message.AutoId
import feh.tec.agents.ReportArchiveGUI.ShowReports
import feh.tec.agents.impl.AgentReports.{StateReportEntry, MessageReport, StateReport}
import feh.tec.agents.impl.{Agent, AgentReport, ReportArchive}
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
    gui foreach (_.updateForms())
  }

  var gui: Option[ReportsGui] = None

  override def processSys = super.processSys orElse{
    case ShowReports(agents) =>
      gui = Some(new ReportsGui(agents))
      gui.get.start()
      gui.get.updateForms()
  }

  class ReportsGui(agents: Seq[AgentRef]) extends SwingAppFrame with Frame9PositionsLayoutBuilder{

    val guis = mutable.HashMap.empty[AgentRef, AgentReportGui]

    val lElems = agents.map{
      ag => guis
        .getOrElse(ag, new AgentReportGui(ag) $$ { guis += ag -> _ })
        .asLayoutElem
    }

    val layout: List[AbstractLayoutSetting] = place(tabs(_.Top, _.Scroll, lElems), "tabs") in theCenter

    def stop() = ???

    def start() = {
      open()
//      size = Toolkit.getDefaultToolkit.getScreenSize
      preferredSize = Toolkit.getDefaultToolkit.getScreenSize
      buildLayout()
    }

    class AgentReportGui(ag: AgentRef) {

      def id(s: String) = s + "#" + ag.hashCode()

      private def cut(a: Any, b: Int, e: Int) = a.toString.drop(b).dropRight(e)

      def getStates = states(ag).toSeq.flatMap{
        case StateReport(of, rep, _) => rep map{
          case (neg, StateReportEntry(p, v, s, e)) =>
            (neg.name, p.get, cut(v, 4, 1), cut(s.map(_.id), 4, 1), e.map(_.toString).getOrElse(""))
        }
      }

      def getMessages = messages(ag).map{
        case MessageReport(to, msg) => (cut(to.id, 3, 1), msg)
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
          scrollable()(guiStates, id("states")).withoutId,
          scrollable()(guiMessages, id("messages")).withoutId
        ).withoutId
      )

      lazy val layout: List[LayoutSetting] =  List(
        place(reports.fillBoth.maxXWeight.maxYWeight, id("reports")) in theCenter,
        place(label(ag.toString), noId) to theNorth of id("reports")
      )

      def asLayoutElem = LayoutElem.unplaced(panel.gridBag(layout: _*), agentName)

      protected def agentName = ag.id match {
        case nme: Agent.IdNamed => nme.name
        case id => id.toString
      }
    }

  }

}


object ReportArchiveGUI{
  case class ShowReports(of: Seq[AgentRef]) extends SystemMessage with AutoId
}