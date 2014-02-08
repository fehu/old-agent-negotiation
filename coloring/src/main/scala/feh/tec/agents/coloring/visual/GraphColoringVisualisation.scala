package feh.tec.agents.coloring.visual

import feh.dsl.swing.AppFrameControl
import feh.tec.agents.coloring.util.Name
import java.awt.Color
import java.util.UUID
import feh.tec.agents.comm.coloring.{ColoringOverseer, ColoringEnvironment, ColoringGraph}
import akka.actor.{Props, ActorSystem, Actor, ActorRef}
import feh.tec.agents.comm.coloring.ColoringOverseer.SetColor
import scala.swing.Frame
import akka.util.Timeout

trait GraphColoringVisualisation extends AppFrameControl{
  protected def graph: ColoringGraph

  def update(name: Name, color: Option[Color])
  def update(id: UUID, color: Option[Color])
  def frame: Frame

  type Visualization
  def graphVisualization: Visualization
}

class ColoringUpdateOverseer(env: ColoringEnvironment, updater: ActorRef)
                            (implicit system: ActorSystem, timeout: Timeout) extends ColoringOverseer(env){
  override protected def props = Props(classOf[ColoringUpdateOverseer.OverseerActor], env, updater)
}

object ColoringUpdateOverseer{
  class OverseerActor(env: ColoringEnvironment, guiUpdater: ActorRef) extends ColoringOverseer.OverseerActor(env){
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
