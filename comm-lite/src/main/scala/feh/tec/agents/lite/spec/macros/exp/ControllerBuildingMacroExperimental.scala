package feh.tec.agents.lite.spec.macros.exp

import feh.tec.agents.lite.spec.macros.ControllerBuildingMacro

import scala.reflect.ClassTag
import scala.reflect.macros.whitebox

trait ControllerBuildingMacroExperimental[C <: whitebox.Context] extends ControllerBuildingMacro[C]{

  final type MacroSegmentsTransform = MacroSegments => MacroSegments
  final def MacroSegmentsTransform(f: MacroSegments => MacroSegments) = f

  object MacroSegments{
    trait Stage

//    object Stage{
//      case object AgentMain extends Stage
//      case object ControllerMain extends Stage
//    }

    def apply(stageApplication: Map[Stage, List[MacroSegment]], extra: Map[String, Any])
             (implicit stagesOrdering: Ordering[Stage]): MacroSegments = MacroSegmentsImpl(stageApplication, extra)

    def empty(implicit stagesOrdering: Ordering[Stage]) = apply(Map(), Map())
  }
  
  trait StagesOrdering extends Ordering[MacroSegments.Stage]{
    def precedence: StagesOrdering.Precedence
  }

  trait StagesOrderingComparisonImmutable extends StagesOrdering{
//    lazy val stages = precedence.toSet.flatMap(p => Set(p.stage, p.comparing))

    def compare(x: MacroSegments.Stage, y: MacroSegments.Stage) = ???
  }

  object StagesOrdering{
    trait Precedence{
      def stages: List[MacroSegments.Stage]
    }

    def apply(_precedence: Precedence): StagesOrdering = new StagesOrdering with StagesOrderingComparisonImmutable {
      final def precedence = _precedence
    }

    implicit def stageIsPrecedence(s: MacroSegments.Stage): Precedence = new Precedence{ val stages = s :: Nil }
    implicit class PrecedenceOps[P <% Precedence](p: P){
      def >>(next: Precedence): Precedence          = new Precedence{ val stages = p.stages ::: next.stages }
      def >>(next: MacroSegments.Stage): Precedence = new Precedence{ val stages = p.stages ::: List(next) }

      def >>:(previous: Precedence): Precedence           = new Precedence{ val stages = previous.stages ::: p.stages }
      def >>:(previous: MacroSegments.Stage): Precedence  = new Precedence{ val stages = previous :: p.stages }

//      def insertAfter(stage: MacroSegments.Stage, after: MacroSegments.Stage): Precedence
    }
  }

  trait MacroSegments extends MacroSegment{
    import MacroSegments._

    /** ordered */
    implicit def stagesOrdering: Ordering[Stage]
    def stageApplication: Map[Stage, List[MacroSegment]] //.withDefault(_ => Nil)

    def append(stage: Stage, f: MacroSegment*): MacroSegments
    def prepend(stage: Stage, f: MacroSegment*): MacroSegments

    def segments = stageApplication.toList.sortBy(_._1).flatMap(_._2)

    def changeExtra[T : ClassTag](name: String, f: Option[T] => Option[T]): MacroSegments

    def getExtra[T : ClassTag](name: String): Option[T]
    def extra[T : ClassTag](name: String) = getExtra[T](name).get

    def apply(v1: Trees) = Function.chain(segments)(v1)
  }

  case class MacroSegmentsImpl(stageApplication: Map[MacroSegments.Stage, List[MacroSegment]], _extra: Map[String, Any])
                              (implicit val stagesOrdering: Ordering[MacroSegments.Stage])
    extends MacroSegments
  {
    def append(stage: MacroSegments.Stage, f: MacroSegment*) = copy(
      stageApplication + (stage -> (stageApplication(stage) ::: f.toList))
    )
    def prepend(stage: MacroSegments.Stage, f: MacroSegment*) = copy(
      stageApplication + (stage -> (f.toList ::: stageApplication(stage)))
    )

    def changeExtra[T: ClassTag](name: String, f: Option[T] => Option[T]): MacroSegments = copy(
      _extra = _extra + (name -> getExtra[T](name).map(t => _extra + (name -> t)).getOrElse(_extra))
    )

    def getExtra[T: ClassTag](name: String) = _extra.get(name).flatMap{
      case t: T => Some(t)
      case _ => None
    }
  }

}
