package feh.tec.agents.lite.spec

import feh.tec.agents
import feh.tec.agents.lite.spec.NegotiationSpecification
import scala.concurrent.duration.FiniteDuration

object NegotiationSpecification{
  sealed trait AbstractVarDef[T]

  case class VarDef[T](name: String, domain: DomainDef[T]) extends AbstractVarDef[T]

  case class VarDefNameless[T](domain: DomainDef[T]) extends AbstractVarDef[T]

  case class NegotiationDef(name: String, issues: Seq[String])

  case class AgentDef(name: String, role: String, negotiations: Seq[AgentNegDef], spec: AgentSpecification)

  trait DomainDef[T]
  case class GenericDomainDef[T, D](domain: D, classOfT: Class[T]) extends DomainDef[T]

  sealed trait Interlocutors
  case class InterlocutorsByRoles(roles: Set[String]) extends Interlocutors

  case class AgentNegDef(negotiation: String,
                         interlocutors: Interlocutors,
                         extra: Seq[AgentNegPartialDef])

  trait ConstraintPart[+T]
  case class ConstraintPartLeaf[+T](part: T) extends ConstraintPart[T]

  case class ConstraintPartComb[+T, +Op] (op: Op, left: ConstraintPart[T], right: ConstraintPart[T]) extends ConstraintPart[T]

  implicit class ConstraintPartLeafOps[T](part: ConstraintPartLeaf[T]){
    def transform[R](f: T => R): ConstraintPartLeaf[R] = part.copy(f(part.part))
  }

  implicit class ConstraintPartCombOps[T, Op](part: ConstraintPartComb[T, Op]){
    def transform[R, OpR](f: T => R, fOp: Op => OpR): ConstraintPartComb[R, OpR] =
      part.copy(fOp(part.op), transformPart(part.left, f, fOp), transformPart(part.right, f, fOp))
  }

  implicit class ConstraintPartOps[T](part: ConstraintPart[T]){
    def transform[R, Op, OpR](f: T => R, fOp: Op => OpR): ConstraintPart[R] = transformPart(part, f, fOp)
  }

  private def transformPart[T, Op, R, OpR](part: ConstraintPart[T], f: T => R, fOp: Op => OpR) = part match {
    case leaf: ConstraintPartLeaf[T] => leaf.transform(f)
    case comb: ConstraintPartComb[T, _] => comb.asInstanceOf[ConstraintPartComb[T, Op]].transform(f, fOp)
  }

  case class AgentConstraintsDef(constraints: Seq[AgentConstraintDef]) extends AgentNegPartialDef{
    def ++(that: AgentConstraintsDef) = AgentConstraintsDef(this.constraints ++ that.constraints)
  }
  case class AgentConstraintDef(name: String, parts: ConstraintPart[AgentVarConstraintDef])
  case class AgentVarConstraintDef(varsOrdered: Seq[ConstraintParamDescription], test: Product => Boolean){
    lazy val vars = varsOrdered.toSet.map((_: NegotiationSpecification.ConstraintParamDescription).varName)
  }
  case class ConstraintParamDescription(tpe: String, varName: String, argName: String)

  sealed trait SpawnDef extends ConfigureDef
  case class SimpleSpawnDef(mp: Map[String, Int]) extends SpawnDef

  sealed trait ConfigureDef
  case class TimeoutsDef(mp: Map[String, FiniteDuration]) extends ConfigureDef
//  case class TimingsDef(mp: Map[String, FiniteDuration]) extends ConfigureDef



  sealed trait AgentNegPartialDef{
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

  import scala.language.experimental.macros

//  def build(dsl: agents.spec.dsl.NegotiationSpecification): NegotiationSpecification = macro agents.spec.macros.NegotiationSpecificationBuilder.build
}

trait NegotiationSpecification { //extends agents.NegotiationSpecification{
  type VarDef = NegotiationSpecification.AbstractVarDef[_]
  type NegotiationDef = NegotiationSpecification.NegotiationDef
  type AgentDef = NegotiationSpecification.AgentDef
  type Config = Set[NegotiationSpecification.ConfigureDef]

  def variables: Seq[VarDef]
  def negotiations: Seq[NegotiationDef]
  def agents: Seq[AgentDef]

  // should be generated by macro
  def spawns   : NegotiationSpecification.SpawnDef
  def timeouts: NegotiationSpecification.TimeoutsDef
  //  def variables
  //  def negotiations
  //  def agents

  def config: Config = Set(spawns, timeouts)

//  override def toString: String =
//    s"""NegotiationSpecification{
//       |  variables:    ${variables.mkString("\n    ", "\n    ", "")}
//       |  negotiations: ${negotiations.mkString("\n    ", "\n    ", "")}
//       |  agents:       ${agents.mkString("\n    ", "\n    ", "")}
//       |}""".stripMargin
}