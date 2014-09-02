package feh.tec.agents.impl

import feh.tec.agents.{NegotiationSpecification => ANegotiationSpecification}
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.runtime.{ universe => ru }


class NegotiationSpecification extends ANegotiationSpecification{
  import feh.tec.agents.impl.NegotiationSpecification.ConstraintBuilder._
  import feh.tec.agents.impl.NegotiationSpecification._

  type Config = {
    def agentSpawn: Seq[SpawnDef]
    def configs: Seq[ConfigureDef]
  }

  implicit protected val specificationBuild = new SpecificationBuild

  protected def define = new Define
  protected def domain = new DomainChooser
  protected def negotiates = new AgentNegDefBuilder
  
  protected def hasConstraints = new AgentConstraintsBuilder 
  protected def proposed = new ConstraintChooser
  protected def equal = Equal

  protected def spawn = new Spawn

  protected def configure = new Configure

  /** language sugar */
  protected def other = identity[String] _

  protected class Define{
    def variable(name: String) = new VarDefBuilder(name)
    def negotiation(name: String) = new NegotiationDefBuilder(name)
    def agent(name: String) = new AgentDefBuilder(name)
  }
  protected class VarDefBuilder(name: String){
    def `with`[T: ru.TypeTag](domain: DomainDef[T]) = VarDef(name, domain)
  }
  protected class DomainChooser{
    def range(r: Range) = DomainRange(r)
    def set[T : ru.TypeTag](s: T*) = DomainSet(s.toSet)
  }
  protected class NegotiationDefBuilder(name: String){
    def over(vars: String*) = NegotiationDef(name, vars)
  }
  protected case class AgentDefBuilder(name: String){
    def withRole(role: String) = new {
      def that(negDefs: AgentNegDef*) = AgentDef(name, role, negDefs)
    }
  }
  protected class AgentNegDefBuilder{
    def the(negotiation: String) = new {
      def `with` (roles: String*) = AgentNegMainDef(negotiation, roles)
    }
  }

  implicit protected def partialDefToDef(pd: AgentNegPartialDef) = pd match {
    case AgentNegMainDef(neg, roles) => AgentNegDef(neg, roles.toSet, Nil)
    case CompositeAgentNegPartialDef(AgentNegMainDef(neg, roles), extra) => AgentNegDef(neg, roles.toSet, extra)
  }


  protected class AgentConstraintsBuilder {
    def over(constraints: AgentConstraintDef*) = AgentConstraintsDef(constraints)
  }
  implicit def issueConstraintDefPairToConstraintBuilder(p: (String, ConstraintDef)) = AgentConstraintDef(p._1, p._2)
  protected class ConstraintChooser{
    def must(cTest: ConstraintTest) = Must(cTest)
    def mustNot(cTest: ConstraintTest) = MustNot(cTest)
  }

  protected class Spawn{
    def agents(mp: (String, Int)*) = SimpleSpawnDef(mp.toMap)
  }

  protected class Configure{
    def timeouts(mp: (String, FiniteDuration)*) = TimeoutsDef(mp.toMap)
    def timings (mp: (String, FiniteDuration)*) = TimingsDef(mp.toMap)
  }

  // extract SpecificationBuild
  lazy val variables    = specificationBuild.collect{ case d: VarDef          => d }
  lazy val negotiations = specificationBuild.collect{ case d: NegotiationDef  => d }
  lazy val agents       = specificationBuild.collect{ case d: AgentDef        => d }
  lazy val config       = new {
    def agentSpawn: Seq[SpawnDef] = specificationBuild.collect{ case d: SimpleSpawnDef  => d }
    def configs: Seq[ConfigureDef] = specificationBuild.collect{ case cd: ConfigureDef => cd }

    override def toString = s"Config(\n\t\tagentSpawn = $agentSpawn.\n\t\tconfigs = $configs)"
  }
}

object NegotiationSpecification{
  protected trait ADef

  case class VarDef(name: String, domain: DomainDef[_])(implicit build: SpecificationBuild)
    extends ANegotiationSpecification.VarDef with ADef { build register this }

  case class NegotiationDef(name: String, issues: Seq[String])(implicit build: SpecificationBuild)
    extends ANegotiationSpecification.NegotiationDef with ADef { build register this }

  case class AgentDef(name: String, role: String, negotiations: Seq[AgentNegDef])
                     (implicit build: SpecificationBuild)
    extends ANegotiationSpecification.AgentDef with ADef { build register this }

  protected abstract class DomainDef[T : ru.TypeTag]{
    val tpe = ru.typeTag[T].tpe
    val clazz = ru.runtimeMirror(getClass.getClassLoader).runtimeClass(tpe)
  }
  case class DomainRange protected[NegotiationSpecification] (range: Range) extends DomainDef[Int]
  case class DomainSet[T: ru.TypeTag] protected[NegotiationSpecification] (set: Set[T]) extends DomainDef[T]

  case class AgentNegDef protected[NegotiationSpecification] (negotiation: String,
                                                              counterpartRoles: Set[String],
                                                              extra: Seq[AgentNegPartialDef])

  protected trait AgentNegPartialDef{
    def and(other: AgentNegPartialDef) = other match{
      case pd@CompositeAgentNegPartialDef(_, conf) => pd.copy(conf = conf :+ other)
    }
  }

  protected case class CompositeAgentNegPartialDef(main: AgentNegMainDef, conf: Seq[AgentNegPartialDef]) extends AgentNegPartialDef{
    override def and(other: AgentNegPartialDef) = other match {
      case pd@CompositeAgentNegPartialDef(_, conf) => copy(conf = this.conf ++ conf)
      case pd => copy(conf = conf :+ other)
    }
  }

  protected case class AgentNegMainDef(negotiation: String, counterPartRoles: Seq[String]) extends AgentNegPartialDef{
    override def and(other: AgentNegPartialDef) = other match{
      case pd: CompositeAgentNegPartialDef => ???
      case pd => CompositeAgentNegPartialDef(this, Seq(pd))
    }
  }

  case class AgentConstraintDef protected[NegotiationSpecification] (issue: String, cDef: ConstraintBuilder.ConstraintDef)
  case class AgentConstraintsDef protected[NegotiationSpecification](contraints: Seq[AgentConstraintDef]) extends AgentNegPartialDef

  protected[agents] object ConstraintBuilder{
    sealed trait ConstraintDef
    sealed trait ConstraintTest
    
    case class Must     protected[NegotiationSpecification](test: ConstraintTest) extends ConstraintDef
    case class MustNot  protected[NegotiationSpecification](test: ConstraintTest) extends ConstraintDef
    
    protected[agents] case object Equal extends ConstraintTest
//    protected case class Code extends ConstraintTest todo
  }

  sealed trait SpawnDef extends ADef
  case class SimpleSpawnDef protected[NegotiationSpecification] (mp: Map[String, Int])(implicit build: SpecificationBuild)
    extends SpawnDef { build register this }

  sealed trait ConfigureDef extends ADef
  case class TimeoutsDef protected[NegotiationSpecification] (mp: Map[String, FiniteDuration])(implicit build: SpecificationBuild)
    extends ConfigureDef { build register this }
  case class TimingsDef protected[NegotiationSpecification] (mp: Map[String, FiniteDuration])(implicit build: SpecificationBuild)
    extends ConfigureDef { build register this }

  protected class SpecificationBuild{
    protected val defs = mutable.Buffer.empty[ADef]

    def register(definition: ADef): Unit = defs += definition
    def get: Seq[ADef] = defs.toSeq
    def collect[R](f: PartialFunction[ADef, R]) = get.collect(f)
  }
}


class NegotiationSpecificationExample extends NegotiationSpecification{

  define variable "v1" `with` domain.range(1 to 10)
  define variable "v2" `with` domain.set('A', 'B', 'C')

  define negotiation "neg-1" over ("v1", "v2", "v5")

  define agent "ag-1" withRole "does something" that (
    negotiates the "neg-1" `with` ("role-1", "role-2") and
      hasConstraints.over(
        "v1" -> (proposed mustNot equal)
      )
    )

  spawn agents(
    "ag-1" -> 10 
    )
  
}