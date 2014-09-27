package feh.tec.agents.impl.agent

import java.util.UUID

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import feh.tec.agents.SystemMessage.RefDemand
import feh.tec.agents._
import feh.tec.agents.impl.Agent
import feh.util._

import scala.concurrent.Await
import scala.reflect.ClassTag

abstract class AgentCreation[Lang <: ProposalLanguage](uuid: UUID,
                                                       negotiationInit: Map[NegotiationId, AgentCreation.NegotiationInit])
  extends Agent[Lang]
{
  def createNegotiation(id: NegotiationId, init: AgentCreation.NegotiationInit): ANegotiation

  lazy val id = Agent.Id(role, uuid)
  lazy val negotiations = negotiationInit.toSet.map((createNegotiation _).tupled)

  id; negotiations // init lazy vals
}


object AgentCreation{
  type Interface = (UUID, Map[NegotiationId, NegotiationInit])

  def props[Ag <: AgentCreation[_] : ClassTag](uuid: UUID,
                                               negotiationInit: Map[NegotiationId, AgentCreation.NegotiationInit]) =
    Props(reflect.classTag[Ag].runtimeClass, uuid, negotiationInit)

  case class NegotiationInit(priority: Priority, issues: Set[Var])

  object Builder extends AgentBuilder[AgentCreation[_], Interface]
}

trait AgentBuilder[-SupAg <: AbstractAgent, -Args <: Product]{
  def props[Ag <: SupAg](args: Args, extracting: Boolean = false)(implicit cTag: ClassTag[Ag]) =
    Props(cTag.runtimeClass, (if(extracting) args.productIterator.toSeq else Seq(args)): _*)
  def create[Ag <: SupAg](args: Args, name: String = null, extracting: Boolean = false)
                         (implicit cTag: ClassTag[Ag], asys: ActorSystem, timeout: Timeout): AgentRef =
  {
    val pr = props(args, extracting)
    val ag = Option(name).map(asys.actorOf(pr, _)) getOrElse asys.actorOf(pr)

    (ag ? RefDemand()).mapTo[AgentRef] |> (Await.result(_, timeout.duration))
  }
}

object AgentBuilder{
  implicit lazy val Default = AgentCreation.Builder
  implicit lazy val PriorityBased = PriorityBasedCreation.Builder
  implicit lazy val GenericIterating = GenericIteratingAgentCreation.Builder

  trait SystemArgs0ServiceBuilder extends AgentBuilder[impl.System.Service.Args0 with AbstractAgent, Tuple0]
  object SystemArgs0ServiceBuilder extends SystemArgs0ServiceBuilder

  trait SystemArgs1ServiceBuilder[T] extends AgentBuilder[impl.System.Service.Args1[T] with AbstractAgent, Tuple1[T]]
  object SystemArgs1ServiceBuilder {
    def apply[T] = new SystemArgs1ServiceBuilder[T]{}
  }

  trait SystemArgs2ServiceBuilder[T1, T2] extends AgentBuilder[impl.System.Service.Args2[T1, T2] with AbstractAgent, (T1, T2)]
  object SystemArgs2ServiceBuilder {
    def apply[T1, T2] = new SystemArgs2ServiceBuilder[T1, T2]{}
  }

}

sealed trait Tuple0 extends Product{
  def productArity = 0
  def productElement(n: Int) = ???
  def canEqual(that: Any) = that.isInstanceOf[Tuple0]
}
object Tuple0 extends Tuple0