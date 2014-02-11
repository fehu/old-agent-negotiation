package feh.tec.agents.coloring.visual

import feh.dsl.swing.AppFrameControl
import feh.tec.agents.coloring.util.Name
import java.awt.Color
import java.util.UUID
import feh.tec.agents.comm.coloring.{ColoringOverseer, ColoringEnvironment, ColoringGraph}
import akka.actor._
import scala.swing.Frame
import akka.util.Timeout
import feh.tec.agents.comm.coloring.ColoringOverseer.SetColor
import scala.concurrent.duration.FiniteDuration

trait GraphColoringVisualisation extends AppFrameControl{
  protected def graph: ColoringGraph

  def update(name: Name, color: Option[Color])
  def update(id: UUID, color: Option[Color])
  def frame: Frame

  type Visualization
  def graphVisualization: Visualization
}

class ColoringUpdateOverseer(env: ColoringEnvironment,
                             updater: ActorRef,
                             ensureDone: FiniteDuration,
                             done: () => Unit)
                            (implicit system: ActorSystem, timeout: Timeout) extends ColoringOverseer(env, ensureDone, done){
  override protected def props =
    Props(classOf[ColoringUpdateOverseer.OverseerActor], env, updater, ensureDone, done, system)
}

object ColoringUpdateOverseer{
  class OverseerActor(env: ColoringEnvironment,
                      guiUpdater: ActorRef,
                      ensureDone: FiniteDuration,
                      done: () => Unit,
                      system: ActorSystem) extends ColoringOverseer.OverseerActor(env, ensureDone, done, system){
    override def receive: Actor.Receive = PartialFunction[Any, Unit]{
      case SetColor(nodeId, color) =>
        env.setColor(nodeId, color)
        guiUpdater ! ColorUpdateActor.SetColor(nodeId, color)
      case other => super.receive(other)
    }
  }
}

object ColorUpdateActor{
  case class SetColor(id: UUID, color: Option[Color])

  class TheActor(app: GraphColoringVisualisation) extends Actor{
    def receive = {
      case SetColor(id, colorOpt) => app.update(id, colorOpt)
    }
  }
  def actor(app: GraphColoringVisualisation)(implicit system: ActorSystem) =
    system.actorOf(Props(classOf[TheActor], app), "ColorUpdateActor")
}
