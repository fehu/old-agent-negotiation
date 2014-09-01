package feh.tec.agents

import java.util.UUID

import akka.util.Timeout
import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.impl.agent.AgentCreation
import AgentCreation.NegotiationInit
import feh.tec.agents.impl._

object NQueen{
  val Size = 4

  object X extends Var("x", _.isInstanceOf[Int]) with Domain.Range{ def domain = 1 to Size }
  object Y extends Var("y", _.isInstanceOf[Int]) with Domain.Range{ def domain = 1 to Size }

  implicit object XYIterator extends DomainIterator.Range()

  def negotiationId = NegotiationId("N-Queen")

//  def init(count: Int) = NegotiationInit(new Priority(count), Set(X, Y))

  def role = Role("Queen")
}

/** old-style */
class NQueen(uuid: UUID,
            negInit: Map[NegotiationId, NegotiationInit],
            val conflictResolver: AgentRef,
            val conflictResolveTimeout: Timeout)
  extends AgentCreation[DefaultNegotiatingLanguage](uuid, negInit) with AgentImpl
{
  import NQueen._

  val role = NQueen.role
  val domainIterators: Map[Var, DomainIterator[Var#Domain, Var#Tpe]] = Map(X.it, Y.it)

  val constraints: Set[Constraint[Var]] = Set(
    CreateConstraint.notEquals(X, negotiationId),
    CreateConstraint.notEquals(Y, negotiationId)
  )
}


class NQueenSpecification(boardSize: Int) extends impl.NegotiationSpecification{
  
  define variable "x" `with` domain.range(1 to boardSize)
  define variable "y" `with` domain.range(1 to boardSize)

  define negotiation "queen's position" over ("x", "y")

  define agent "Queen" withRole "chess queen" that(
    negotiates the "queen's position" `with` other("chess queen") and
      hasConstraints.over(
        "x" -> (proposed mustNot equal),
        "y" -> (proposed mustNot equal)
      )
    )

  spawn agents(
    "Queen" -> boardSize
    )
}