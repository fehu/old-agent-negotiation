package feh.tec.agents

import akka.actor.ActorRef

/** define the negotiation environment */
trait NegotiationSpecification{
  import NegotiationSpecification._

  type Config

  def variables: Seq[VarDef]
  def negotiations: Seq[NegotiationDef]
  def agents: Seq[AgentDef]

  def config: Config
}

object NegotiationSpecification{
  trait VarDef
  trait AgentDef
  trait NegotiationDef
}


/** build the negotiation environment(controller) by spec */
trait NegotiationControllerBuilder[Spec <: NegotiationSpecification, Control <: NegotiationController] extends (Spec => ActorRef)