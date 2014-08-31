package feh.tec.agents

import java.awt.{Color => JColor}
import java.util.UUID

import akka.util.Timeout
import feh.tec.agents.impl.AgentCreation.NegotiationInit
import feh.tec.agents.impl.{AgentCreation, DefaultNegotiatingLanguage}

object Coloring{

  object Color extends Var("Color", _.isInstanceOf[JColor]) with Domain.Small[JColor]{
    def domain = Set(JColor.red, JColor.green, JColor.blue)
  }

  implicit object ColorIterator extends DomainIterator.Generic[JColor]

  val role = Role("Coloring")
  def negotiationId = NegotiationId("Graph Coloring")
}

class Coloring(uuid: UUID,
               negInit: Map[NegotiationId, NegotiationInit],
               val conflictResolver: AgentRef,
               val conflictResolveTimeout: Timeout)
  extends AgentCreation[DefaultNegotiatingLanguage](uuid, negInit) with AgentImpl
{
  import Coloring._

  val role: Role = Coloring.role
  val vars: Set[Var] = Set(Color)
  val domainIterators = Map(Color.it)
  val constraints = Set( CreateConstraint.notEquals(Color, negotiationId) )
}