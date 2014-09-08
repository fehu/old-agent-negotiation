package feh.tec.agents.impl

import scala.concurrent.duration.FiniteDuration

object NegotiationSpecification{
  sealed trait AbstractVarDef[T]

  case class VarDef[T](name: String, domain: DomainDef[T]) extends AbstractVarDef[T]

  case class VarDefNameless[T](domain: DomainDef[T]) extends AbstractVarDef[T]

  case class NegotiationDef(name: String, issues: Seq[String])

  case class AgentDef(name: String, role: String, negotiations: Seq[AgentNegDef])

  trait DomainDef[T] // todo
  //  case class DomainRange protected[NegotiationSpecification] (range: Range) extends DomainDef[Int]
  //  case class DomainSet[T: ru.TypeTag] protected[NegotiationSpecification] (set: Set[T]) extends DomainDef[T]

  case class AgentNegDef protected[NegotiationSpecification] (negotiation: String,
                                                              counterpartRoles: Set[String],
                                                              extra: Seq[AgentNegPartialDef])

  case class AgentConstraintDef[T] protected[NegotiationSpecification](v: VarDefNameless[T], test: (T, T) => Boolean)
  case class AgentConstraintsDef protected[NegotiationSpecification](contraints: Seq[AgentConstraintDef[_]]) extends AgentNegPartialDef

  sealed trait SpawnDef
  case class SimpleSpawnDef protected[NegotiationSpecification] (mp: Map[String, Int]) extends SpawnDef

  sealed trait ConfigureDef
  case class TimeoutsDef protected[NegotiationSpecification] (mp: Map[String, FiniteDuration]) extends ConfigureDef
  case class TimingsDef protected[NegotiationSpecification] (mp: Map[String, FiniteDuration]) extends ConfigureDef



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
}
