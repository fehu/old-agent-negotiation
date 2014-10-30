package feh.tec.agents.light.impl.agent

import feh.tec.agents.light.{Domain, AgentCreationInterface, DomainIteratorBuilder, Var}
import feh.tec.agents.light.impl.DomainIterating
import feh.util._

trait DomainIteratorsDefault {
  self: DomainIterating[_]  =>

  lazy val domainIterators: Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]] = negotiations.flatMap(_.issues).zipMap{
    case range: Domain.Range => new DomainIteratorBuilder.Range()
  }.toMap.mapValues(_.asInstanceOf[DomainIteratorBuilder[Var#Domain, Var#Tpe]])
}
