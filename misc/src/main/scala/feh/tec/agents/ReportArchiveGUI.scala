package feh.tec.agents

import java.awt.Toolkit

import akka.actor.ActorRef
import feh.dsl.swing.SwingAppBuildingEnvironment
import feh.tec.agents.Message.AutoId
import feh.tec.agents.ReportArchiveGUI.ShowReports
import feh.tec.agents.impl.AgentReports.{MessageReport, StateReport, StateReportEntry}
import feh.tec.agents.impl.{Agent, AgentReport, ReportArchive}
import feh.util._

import scala.collection.mutable
import scala.concurrent.duration._

// todo
class ReportArchiveGUI extends ReportArchive with SwingAppBuildingEnvironment{

  var controller: ActorRef = null

  lazy val states   = mutable.HashMap.empty[AgentRef, mutable.Buffer[StateReport]] withDefault(_ => mutable.Buffer.empty)
  lazy val messages = mutable.HashMap.empty[AgentRef, mutable.Buffer[MessageReport]] withDefault(_ => mutable.Buffer.empty)

  def reports = states.zipByKey(messages).mapValues[List[AgentReport]]{case (x, y) => x.toList ::: y. toList}

  def repSize = states.zipByKey(messages).mapValues{
    case (bs, bm) => bs.size.ensuring(_ == bm.size)
  } 
  
  private val lastSize = mutable.HashMap.empty[AgentRef, Int]

  def newReport(rep: AgentReport) = {
    log.info("!!!!!!!newReport")
    rep match{
      case rep: StateReport => states <<= (rep.of, _ += rep)
      case rep: MessageReport => messages <<= (rep.of, _ += rep)
    }
  }

  var gui: Option[ReportsGui] = None

  case object Upd extends SystemMessage{ def id = ??? }

  override def processSys = ({
    case start@SystemMessage.Start() =>
      controller = sender()
    case ShowReports(agents, updFreq) =>
      gui = Some(new ReportsGui(agents))
      gui.get.start()
      context.system.scheduler.schedule(0 millis, updFreq, self, Upd)(context.system.dispatcher, self)
    case Upd if lastSize.toMap == repSize => // do nothing
    case Upd =>
        lastSize ++= repSize
        gui foreach (_.updateForms())
  }: PartialFunction[SystemMessage, Unit]) orElse super.processSys

  class ReportsGui(agents: Seq[AgentRef]) extends SwingAppFrame with Frame9PositionsLayoutBuilder{

    val guis = mutable.HashMap.empty[AgentRef, AgentReportGui]

    val lElems = agents.map{
      ag => guis
        .getOrElse(ag, new AgentReportGui(ag) $$ { guis += ag -> _ })
        .asLayoutElem
    }

    lazy val stopButton: DSLButtonBuilder = triggerFor{
      controller ! SystemMessage.Stop()
      stopButton.enabled = false
      resumeButton.enabled = true
    }.button("Stop")

    lazy val resumeButton: DSLButtonBuilder = triggerFor{
      controller ! SystemMessage.Resume()
      stopButton.enabled = true
      resumeButton.enabled = false
    }.button("Resume").affect(_.enabled = false)

    val buttonsPanel = panel.flow(_.Left)(stopButton -> "stop", resumeButton -> "resume")
      .anchor(_.West)

    val theTabs = tabs(_.Top, _.Scroll, lElems)
      .maxXWeight.maxYWeight.fillBoth

    val layout: List[AbstractLayoutSetting] = List(
      place(theTabs, "tabs") in theCenter,
      place(buttonsPanel, "buttons") in theNorth
    )

    def stop() = ???

    def start() = {
      open()
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
  case class ShowReports(of: Seq[AgentRef], updateFreq: FiniteDuration) extends SystemMessage with AutoId
}