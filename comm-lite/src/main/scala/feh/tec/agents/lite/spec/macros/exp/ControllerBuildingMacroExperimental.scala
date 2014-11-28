package feh.tec.agents.lite.spec.macros.exp

import feh.tec.agents.lite.spec.macros.ControllerBuildingMacro

import scala.reflect.macros.whitebox

trait ControllerBuildingMacroExperimental[C <: whitebox.Context] extends ControllerBuildingMacro[C]{

  final type MacroSegmentsTransform = MacroSegments => MacroSegments

  object MacroSegments{
    abstract class Stage/*(val priority: Int) extends Ordered[Stage] {
      def compare(that: Stage) = this.priority compare that.priority
    }*/

    object Stage{
      case object AgentMain extends Stage//(-10000)
      case object ControllerMain extends Stage//(1000)
    }

    def apply(stageApplication: Map[Stage, List[MacroSegment]])
             (implicit stagesAreOrdering: Ordering[Stage]): MacroSegments = MacroSegmentsImpl(stageApplication)

    def empty(implicit stagesAreOrdering: Ordering[Stage]) = apply(Map())
  }

  trait MacroSegments extends MacroSegment{
    import MacroSegments._

    /** ordered */
    implicit def stagesAreOrdering: Ordering[Stage]
    def stageApplication: Map[Stage, List[MacroSegment]] //.withDefault(_ => Nil)

    def append(stage: Stage, f: MacroSegment*): MacroSegments
    def prepend(stage: Stage, f: MacroSegment*): MacroSegments

    def segments = stageApplication.toList.sortBy(_._1).flatMap(_._2)

    def apply(v1: Trees) = Function.chain(segments)(v1)
  }

  case class MacroSegmentsImpl(stageApplication: Map[MacroSegments.Stage, List[MacroSegment]])
                                    (implicit val stagesAreOrdering: Ordering[MacroSegments.Stage])
    extends MacroSegments
  {
    def append(stage: MacroSegments.Stage, f: MacroSegment*) = copy(
      stageApplication + (stage -> (stageApplication(stage) ::: f.toList))
    )
    def prepend(stage: MacroSegments.Stage, f: MacroSegment*) = copy(
      stageApplication + (stage -> (f.toList ::: stageApplication(stage)))
    )
  }

}
