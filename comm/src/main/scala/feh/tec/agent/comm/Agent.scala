package feh.tec.agent.comm

import scala.collection.mutable
import akka.actor.{Scheduler, Actor, ActorRef}
import feh.util._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext
import akka.pattern.ask
import akka.util.Timeout

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
      Response(Option(tellSender).toSeq, Option(tellAll).toSeq, Option(affectEnvironment).toSeq)
  }

  object Response{
    def build[Lang <: Language, Env] = new ResponseBuilder[Lang, Env]

    def apply[Lang <: Language, Env](tellSender: Lang#Expr = null.asInstanceOf[Lang#Expr],
                                     tellAll: Lang#Expr = null.asInstanceOf[Lang#Expr],
                                     affectEnvironment: EnvironmentRef[Env] => Unit = null): Response[Lang, Env] =
      Response(Option(tellSender).toSeq, Option(tellAll).toSeq, Option(affectEnvironment).toSeq)
  }

  case class Response[Lang <: Language, Env] protected[Agent] (tellSender: Seq[Lang#Expr],
                                                               tellAll: Seq[Lang#Expr],
                                                               affectEnvironment: Seq[EnvironmentRef[Env] => Unit]){
    def append(b: ResponseBuilder[Lang, Env] => Response[Lang, Env]) ={
      val resp = b(new ResponseBuilder[Lang, Env])
        copy(
          tellSender = tellSender ++ resp.tellSender,
          tellAll = tellAll ++ resp.tellAll,
          affectEnvironment = affectEnvironment ++ resp.affectEnvironment
        )
    }
    def + = append _
  }
  
  
  object Communicating{
    abstract class AgentActor[Id, Lang <: Language, Env](val id: Id,
                                                         val controller: CommAgentController[Lang, Env, _],
                                                         val envRef: EnvironmentRef[Env]) extends Actor{
      def respond: (Id, Lang#Expr) => Response[Lang, Env]
      def isLangExpr(a: Any): Boolean

      def onReceive(msg: Lang#Expr)

      protected def sendResponses(sender: ActorRef, resp: Response[Lang, Env]){
        val Response(snd, all, affect) = resp
        snd foreach (sender !)
        all foreach controller.tellAll
        affect foreach (_(envRef))
      }

      def receive = {
        case expr if isLangExpr(expr) => {
          onReceive(expr.asInstanceOf[Lang#Expr])
          val resp = respond(controller.id(sender).asInstanceOf[Id], expr.asInstanceOf[Lang#Expr])
          sendResponses(sender, resp)
        }
      }
    }

    trait ResponseDelay[Id, Lang <: Language, Env] extends AgentActor[Id, Lang, Env] {
      protected def scheduler: Scheduler
      def messageDelay: FiniteDuration

      protected implicit def execContext: ExecutionContext

      protected case class Delayed(id: Id, expr: Lang#Expr)

      override def receive = {
        case Delayed(i, expr) =>
          sendResponses(controller.ref(i.cast), respond(i, expr))
        case expr if isLangExpr(expr) =>
          scheduler
          .scheduleOnce(messageDelay, self, Delayed(controller.id(sender).asInstanceOf[Id], expr.asInstanceOf[Lang#Expr]))
        case other => super.receive(other)
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

    def askAny(id: Ag#Id, msg: Any)(implicit timeout: Timeout) = ref(id) ? msg
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
