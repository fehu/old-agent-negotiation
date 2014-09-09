package feh.tec.agents

import akka.actor.ActorRef



/** build the negotiation environment(controller) by spec */
trait NegotiationControllerBuilder[Spec <: NegotiationSpecification, Control <: NegotiationController] extends (Spec => ActorRef)