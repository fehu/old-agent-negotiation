package feh.tec.agent.comm

import java.util.UUID
import scala.collection.mutable
import akka.actor.{Actor, ActorRef}
import feh.util._

trait AbstractAgent

trait Agent[Env] extends AbstractAgent{
  def interaction: EnvironmentRef[Env] => EnvironmentEffect[Env]
}

trait EnvironmentRef[Env]
trait EnvironmentEffect[Env]

object Agent{

  trait Communicating[Lang <: Language, Env] extends AbstractAgent with Actor{
    def id: UUID

    type EnvRef <: EnvironmentRef[Env]

    type RespondArg = (Lang#Expr, EnvRef)
    def respond: RespondArg => Option[Lang#Expr]

    def envRef: EnvRef
    def controller: CommAgentController[Lang, Env, _]

    def receive: Actor.Receive = {
      case expr: Lang#Expr => respond((expr, envRef)) map (sender !)
    }
  }


  trait Negotiating[Lang <: NegotiatingLanguage, Env] extends Communicating[Lang, Env]

  trait CommAgentController[Lang <: Language, Env, Ag <: Communicating[Lang, Env]]{
    protected var agents = mutable.Map.empty[UUID, Ag]
    protected var refs = mutable.Map.empty[ActorRef, UUID]

    def register(agent: Ag) = {
      agents += agent.id -> agent
      refs += agent.self -> agent.id
    }

    def ref(id: UUID) = agents(id).self
    def id(ref: ActorRef): UUID = refs(ref)

    def tell(id: UUID, msg: Lang#Expr)(implicit sender: ActorRef): ActorRef = ref(id) $$ (_.tell(msg, sender))
  }

}

trait Language{
  type Expr
}

trait NegotiatingLanguage extends Language{
  type Issue <: Expr
  type Accepted <: Expr
  type Rejected <: Expr
}
