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
    type Id
    def id: Id

    type EnvRef <: EnvironmentRef[Env]

    object Response{
      def apply(tellSender: Lang#Expr = null.asInstanceOf[Lang#Expr],
                tellAll: Lang#Expr = null.asInstanceOf[Lang#Expr],
                affectEnvironment: EnvRef => Unit = null): Response =
        Response(Option(tellSender), Option(tellAll), Option(affectEnvironment))
    }

    case class Response protected (tellSender: Option[Lang#Expr],
                                   tellAll: Option[Lang#Expr],
                                   affectEnvironment: Option[EnvRef => Unit])

    def respond: Lang#Expr => Response

    def envRef: EnvRef
    def controller: CommAgentController[Lang, Env, _]

    def isLangExpr(a: Any): Boolean

    def receive: Actor.Receive = {
      case expr if isLangExpr(expr) => {
        println(s"Lang#Expr $expr")

        val Response(resp, all, affect) = respond(expr.asInstanceOf[Lang#Expr])
        resp foreach (sender !)
        all foreach controller.tellAll
        affect foreach (_(envRef))
      }
    }
  }


  trait Negotiating[Lang <: NegotiatingLanguage, Env] extends Communicating[Lang, Env]

  trait CommAgentController[Lang <: Language, Env, Ag <: Communicating[Lang, Env]]{
    protected var agents = mutable.Map.empty[Ag#Id, Ag]
    protected var refs = mutable.Map.empty[ActorRef, Ag#Id]

    def register(agent: Ag) = {
      agents += agent.id -> agent
      refs += agent.self -> agent.id
    }

    def ref(id: Ag#Id) = agents(id).self
    def id(ref: ActorRef): Ag#Id = refs(ref)

    def tell(id: Ag#Id, msg: Lang#Expr)(implicit sender: ActorRef): ActorRef = ref(id) $$ (_.tell(msg, sender))
    def tellAll(msg: Lang#Expr)(implicit sender: ActorRef)
  }

}

trait Language{
  type Expr <: AnyRef
}

trait NegotiatingLanguage extends Language{
  type Issue <: Expr
  type Accepted <: Expr
  type Rejected <: Expr
}
