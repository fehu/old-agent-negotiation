package feh.tec.agents.impl.agent

import java.util.UUID

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import feh.tec.agents.SystemMessage.RefDemand
import feh.tec.agents.impl.{AgentRef, Agent}
import feh.tec.agents._
import akka.pattern.ask
import feh.util._

import scala.concurrent.Await
import scala.reflect.ClassTag

abstract class AgentCreation[Lang <: ProposalLanguage](uuid: UUID,
                                                       negotiationInit: Map[NegotiationId, AgentCreation.NegotiationInit])
  extends Agent[Lang]
{
  def createNegotiation(id: NegotiationId, init: AgentCreation.NegotiationInit): ANegotiation

  val id = Agent.Id(role, uuid)
  lazy val negotiations = negotiationInit.toSet.map((createNegotiation _).tupled)

}


object AgentCreation{
  type Interface = (UUID, Map[NegotiationId, NegotiationInit])

  def props[Ag <: AgentCreation[_] : ClassTag](uuid: UUID,
                                               negotiationInit: Map[NegotiationId, AgentCreation.NegotiationInit]) =
    Props(reflect.classTag[Ag].runtimeClass, uuid, negotiationInit)

  case class NegotiationInit(priority: Priority, issues: Set[Var])

  object Builder extends AgentBuilder[AgentCreation[_], Interface]
}

trait AgentBuilder[Ag <: AbstractAgent, Args <: Product]{
  def props(args: Args)(implicit cTag: ClassTag[Ag]) = Props(cTag.runtimeClass, args.productIterator.toSeq: _*)
  def create(args: Args)(implicit cTag: ClassTag[Ag], asys: ActorSystem, timeout: Timeout): AgentRef =
    (asys.actorOf(props(args)) ? RefDemand).mapTo[AgentRef] |> (Await.result(_, timeout.duration))
}

object AgentBuilder{
  implicit lazy val Default = AgentCreation.Builder
  implicit lazy val PriorityBased = PriorityBasedCreation.Builder

  implicit object SystemArgs0Service extends AgentBuilder[impl.System.Service.Args0 with AbstractAgent, Tuple0]

}

sealed trait Tuple0 extends Product{
  def productArity = 0
  def productElement(n: Int) = ???
  def canEqual(that: Any) = that.isInstanceOf[Tuple0]
}
object Tuple0 extends Tuple0