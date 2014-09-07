package feh.tec.agents

import java.util.UUID

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.impl._
import feh.tec.agents.impl.agent.AgentCreation
import feh.tec.agents.impl.agent.AgentCreation.NegotiationInit

import scala.concurrent.duration._

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

  def checkConstraintsRepeat = ???
}


class NQueenSpecification(boardSize: Int) extends impl.NegotiationSpecification{
  
  val x = variable `with` domain.range(1 to boardSize)
  val y = variable `with` domain.range(1 to boardSize)

  define negotiation "queen's position" over ("x", "y")

  define agent "Queen" withRole "chess queen" that(
    negotiates the "queen's position" `with` other("chess queen") and
      hasConstraints.over(
        x >> { _ => proposed != value },
        y >> { _ => proposed != value }
      )
    )

  spawn agents(
    "Queen" -> boardSize
    )

  configure timeouts(
    "creation" -> 100.millis
//    "resolve conflict" -> 100.millis
    )
}

object NQueenApp extends App{
  implicit val acSys = ActorSystem.create("NQueenApp")

  acSys.actorOf(Props(classOf[NQueenUserAgent]), "NQueenUserAgent")
}

class NQueenUserAgent extends UserAgent{
  def name = "user"

  implicit def acSys = context.system
  implicit def exContext = context.dispatcher

  val spec = new NQueenSpecification(4)

  println("spec.variables = " + spec.variables)
  println("spec.negotiations = " + spec.negotiations)
  println("spec.agents = " + spec.agents)
  println("spec.config = " + spec.config)

  val builder = new ControllerBuilder[GenericNegotiatingAgentImpl]
  val controller = builder(spec)

  println("builder.vars = " + builder.vars)
  println("builder.negotiations = " + builder.negotiations)
  println("builder.agents = " + builder.agents)
  println("builder.agentsCount = " + builder.agentsCount)

  println("starting")
  controller ! SystemMessage.Start()

  implicit def timeout = Timeout(300 millis)

  def agFuture = (controller ? impl.NegotiationController.Request.AgentRefs()).mapTo[Seq[AgentRef]]

  agFuture.map{
    agents => controller ! Controller.ShowReportsGui(agents, silence = true, updateFreq = 200 millis)
  }

}