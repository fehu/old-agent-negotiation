package feh.tec.agents

import java.util.UUID

import akka.actor.{Props, ActorSystem}
import akka.util.Timeout
import feh.tec.agents.ConstraintsView.Constraint
import feh.tec.agents.SystemMessage.StateReportEntry
import feh.tec.agents.impl.agent.AgentCreation
import AgentCreation.NegotiationInit
import feh.tec.agents.impl._
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.pattern.ask

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

  configure timeouts(
    "creation" -> 30.millis
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

  val builder = new NegotiationControllerBuilder.Default[GenericNegotiatingAgentImpl]
  val controller = builder(spec)

  println("builder.vars = " + builder.vars)
  println("builder.negotiations = " + builder.negotiations)
  println("builder.agents = " + builder.agents)
  println("builder.agentsCount = " + builder.agentsCount)

  println("starting")
  controller ! SystemMessage.Start

  implicit def timeout = Timeout(300 millis)

  def agF = (controller ? impl.NegotiationController.Request.AgentRefs()).mapTo[Seq[AgentRef]]

  Thread.sleep(500)

  def agents = Await.result(agF, 300 millis)

  context.system.scheduler.schedule(0 millis, 1 second, self, "printReports")

  def printReports() = agents.map(impl.System.allNegotiationReports).map(_.map{
    msg =>
      val sb = new StringBuilder
      sb ++= s"Report by ${msg.of.id}:\n"
      msg.report foreach {
        case (negId, StateReportEntry(p, v, s, extra)) =>
          sb ++= (" "*12 + s"priority: $p\n")
          sb ++= (" "*12 + s"  values: $v\n")
          sb ++= (" "*12 + s"   scope: $s")
          if(extra.isDefined) sb ++= ("\n" + " "*12 + s"   extra: ${extra.get}")
      }
      println(sb.mkString)
  })

  override def receive = ({
    case "printReports" =>
//      println(agents)
      printReports()
  }: PartialFunction[Any, Unit]) orElse super.receive
}