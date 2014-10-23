package feh.tec.agents.light.spec

import feh.tec.agents
import feh.tec.agents.light.spec.AgentProps.AgentPropsBundle
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

  case class AgentConstraintDef(name: String, v: Seq[ConstraintParamDescription], test: Product => Boolean)
  case class AgentConstraintsDef(constraints: Seq[AgentConstraintDef]) extends AgentNegPartialDef

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