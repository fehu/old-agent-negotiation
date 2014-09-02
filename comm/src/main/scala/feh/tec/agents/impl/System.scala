package feh.tec.agents.impl

import java.util.UUID

import akka.actor.Props
import akka.util.Timeout
import feh.tec.agents._
import akka.pattern.ask
import feh.tec.agents.impl.Agent.EssentialSystemSupport

import scala.concurrent.ExecutionContext

object System {
  def negotiationReports(ag: AgentRef, of: NegotiationId*)
                        (implicit xc: ExecutionContext, askTimeout: Timeout) =
    (ag.ref ? SystemMessage.ReportStates(of: _*)).mapTo[SystemMessage.StateReport]

  def allNegotiationReports(ag: AgentRef)
                           (implicit xc: ExecutionContext, askTimeout: Timeout) =
    (ag.ref ? SystemMessage.ReportAllStates()).mapTo[SystemMessage.StateReport]
  
  object Service{
    sealed trait ArgsArity{ self: AbstractAgent => }

    trait Args0 extends ArgsArity{ self: AbstractAgent => }
    trait Args1[T] extends ArgsArity{ self: AbstractAgent => }
  }

  def conflictResolver = Props(classOf[ConflictResolver])

  class ConflictResolver extends SystemAgent with service.ConflictResolver with Service.Args0{
    override def lifeCycle: PartialFunction[AbstractMessage, Unit] =
      super[SystemAgent].lifeCycle orElse super[ConflictResolver].lifeCycle
  }
}

trait SystemAgent extends AbstractAgent with EssentialSystemSupport{
  def role: SystemRole

  type Id = Agent.Id
  val id = Agent.Id(role, UUID.randomUUID())
  implicit val ref = AgentRef(id, self)

  def lifeCycle: PartialFunction[AbstractMessage, Unit] = {
    case sys: SystemMessage => processSys(sys)
  }
}

trait UserAgent extends AbstractAgent{
  agent =>

  type Id = Agent.Id

  def name: String

  val role = new UserRole { val name = agent.name }

  val id = Agent.Id(role, UUID.randomUUID())
  implicit val ref = AgentRef(id, self)

  def lifeCycle: PartialFunction[AbstractMessage, Unit] = {
    case msg => println(s"received by User($name): $msg")
  }
}
