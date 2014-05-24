package feh.tec.agent.oldcomm

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
  protected class ResponseBuilder[Lang <: Language, Env, Id]{
    def apply(tellSender: Lang#Expr = null.asInstanceOf[Lang#Expr],
              tellAll: Lang#Expr = null.asInstanceOf[Lang#Expr],
              personal: (Id, Lang#Expr) = null.asInstanceOf[(Id, Lang#Expr)],
              affectEnvironment: EnvironmentRef[Env] => Unit = null): Response[Lang, Env, Id] =
      Response(Option(tellSender).toSeq, Option(tellAll).toSeq, Option(personal).toSeq, Option(affectEnvironment).toSeq)
    def seq(tellSender: List[Lang#Expr] = Nil,
              tellAll: List[Lang#Expr] = Nil,
              personal: List[(Id, Lang#Expr)] = Nil,
              affectEnvironment: List[EnvironmentRef[Env] => Unit] = Nil): Response[Lang, Env, Id] =
      Response(tellSender, tellAll, personal, affectEnvironment)
  }

  object Response{
    def build[Lang <: Language, Env, Id] = new ResponseBuilder[Lang, Env, Id]

    def apply[Lang <: Language, Env, Id](
                                     tellSender: Lang#Expr = null.asInstanceOf[Lang#Expr],
                                     tellAll: Lang#Expr = null.asInstanceOf[Lang#Expr],
                                     personal: (Id, Lang#Expr) = null.asInstanceOf[(Id, Lang#Expr)],
                                     affectEnvironment: EnvironmentRef[Env] => Unit = null): Response[Lang, Env, Id] =
      Response(Option(tellSender).toSeq, Option(tellAll).toSeq, Option(personal).toSeq, Option(affectEnvironment).toSeq)
  }

  case class Response[Lang <: Language, Env, Id] protected[Agent] (
                     tellSender: Seq[Lang#Expr],
                     tellAll: Seq[Lang#Expr], 
                     personal: Seq[(Id, Lang#Expr)],
                     affectEnvironment: Seq[EnvironmentRef[Env] => Unit]){
    def append(b: ResponseBuilder[Lang, Env, Id] => Response[Lang, Env, Id]) ={
      val resp = b(new ResponseBuilder[Lang, Env, Id])
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
                                                         val controller: CommAgentController[Lang, Env, Id],
                                                         val envRef: EnvironmentRef[Env]) extends Actor{
      def respond: (Id, Lang#Expr) => Response[Lang, Env, Id]
      def isLangExpr(a: Any): Boolean

      def onReceive(msg: Lang#Expr)

      protected def sendResponses(sender: ActorRef, resp: Response[Lang, Env, Id]){
        val Response(snd, all, pm, affect) = resp
        snd foreach (sender !)
        all foreach controller.tellAll
        pm.foreach{ case (i: Id, msg) => controller.tell(i, msg) }
        affect foreach (_(envRef))
      }

      def receive = {
        case expr if isLangExpr(expr) => {
          onReceive(expr.asInstanceOf[Lang#Expr])
          val resp = respond(controller.id(sender), expr.asInstanceOf[Lang#Expr])
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
          .scheduleOnce(messageDelay, self, Delayed(controller.id(sender), expr.asInstanceOf[Lang#Expr]))
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
    def controller: CommAgentController[Lang, Env, Id]

    def actor: ActorRef
  }


  trait Negotiating[Lang <: NegotiatingLanguage, Env] extends Communicating[Lang, Env]

  trait CommAgentController[Lang <: Language, Env, Id]{
    private type myId = Id
    type Agent = Communicating[Lang, Env]{ type Id = myId }

    protected var agents = mutable.Map.empty[Id, Agent]
    protected var refs = mutable.Map.empty[ActorRef, Id]

    def register(agent: Agent) = {
      agents += agent.id -> agent
      refs += agent.actor -> agent.id
    }

    def ref(id: Id) = agents(id).actor
    def id(ref: ActorRef): Id = refs(ref)

    def askAny(id: Id, msg: Any)(implicit timeout: Timeout) = ref(id) ? msg
    def tell(id: Id, msg: Lang#Expr)(implicit sender: ActorRef): ActorRef = ref(id) $$ (_.tell(msg, sender))
    def tellAll(msg: Lang#Expr)(implicit sender: ActorRef)

    def allRefs = refs.keys
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
