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
  protected class ResponseBuilder[Lang <: Language, Env]{
    def apply(tellSender: Lang#Expr = null.asInstanceOf[Lang#Expr],
              tellAll: Lang#Expr = null.asInstanceOf[Lang#Expr],
              affectEnvironment: EnvironmentRef[Env] => Unit = null): Response[Lang, Env] =
      Response(Option(tellSender), Option(tellAll), Option(affectEnvironment))
  }

  object Response{
    def build[Lang <: Language, Env] = new ResponseBuilder[Lang, Env]

    def apply[Lang <: Language, Env](tellSender: Lang#Expr = null.asInstanceOf[Lang#Expr],
                                     tellAll: Lang#Expr = null.asInstanceOf[Lang#Expr],
                                     affectEnvironment: EnvironmentRef[Env] => Unit = null): Response[Lang, Env] =
      Response(Option(tellSender), Option(tellAll), Option(affectEnvironment))
  }

  case class Response[Lang <: Language, Env] protected[Agent] (tellSender: Option[Lang#Expr],
                                                               tellAll: Option[Lang#Expr],
                                                               affectEnvironment: Option[EnvironmentRef[Env] => Unit])
  
  
  object Communicating{
    abstract class AgentActor[Id, Lang <: Language, Env](val id: Id,
                                                         val controller: CommAgentController[Lang, Env, _],
                                                         val envRef: EnvironmentRef[Env]) extends Actor{
      def respond: (Id, Lang#Expr) => Response[Lang, Env]
      def isLangExpr(a: Any): Boolean

      def onReceive(msg: Lang#Expr)

      def receive = {
        case expr if isLangExpr(expr) => {
          onReceive(expr.asInstanceOf[Lang#Expr])

          val Response(resp, all, affect) = respond(controller.id(sender).asInstanceOf[Id], expr.asInstanceOf[Lang#Expr])
          resp foreach (sender !)
          all foreach controller.tellAll
          affect foreach (_(envRef))
        }
      }
    }

  }
  trait Communicating[Lang <: Language, Env] extends AbstractAgent{
    self =>

    type Id
    def id: Id

    type EnvRef <: EnvironmentRef[Env]

    def envRef: EnvRef
    def controller: CommAgentController[Lang, Env, _]

    def actor: ActorRef
  }


  trait Negotiating[Lang <: NegotiatingLanguage, Env] extends Communicating[Lang, Env]

  trait CommAgentController[Lang <: Language, Env, Ag <: Communicating[Lang, Env]]{
    protected var agents = mutable.Map.empty[Ag#Id, Ag]
    protected var refs = mutable.Map.empty[ActorRef, Ag#Id]

    def register(agent: Ag) = {
      agents += agent.id -> agent
      refs += agent.actor -> agent.id
    }

    def ref(id: Ag#Id) = agents(id).actor
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
