package feh.tec.agents.impl

import java.util.UUID
import Language.dsl._
import feh.tec.agents
import feh.util._

import feh.tec.agents._

object Agent{
  case class Id(role: Role, uuid: UUID)

  trait SystemSupport {
    self: NegotiatingAgent[_] =>

    def process(sys: SystemMessage) = ???
  }
}


trait PriorityBasedBacktrack[Lang <: BacktrackLanguage with agents.Language.Priority]
  extends NegotiatingAgent[Lang] with BackTracking[Lang] with Language.Builder[Lang]
{
  type Id = Agent.Id

  def process(p: Message) = ???

  def onProposal(msg: Lang#Proposal) = ???
  def onAccepted(msg: Lang#Accepted) = ???
  def onRejected(msg: Lang#Rejected) = ???
  def onFallback(msg: Lang#Fallback) = ???

  def createProposal(negotiation: Negotiation) = build[Lang#Proposal](negotiation, I propose I set (
    negotiation.vals.toSeq map {
      case (issue, value) => issue -> issue.cast(value).getOrThrow(s"wrong value $value for $issue")
    } : _*))
  def createAccepted(negotiation: Negotiation) = buildMessage(negotiation.id, I.accept).asInstanceOf[Lang#Accepted]
  def createRejected(negotiation: Negotiation) = buildMessage(negotiation.id, I.reject).asInstanceOf[Lang#Rejected]
  def createFallback(negotiation: Negotiation) = ???

  private def build[T <: Lang#Msg](neg: Negotiation, expr: Language.Buildable) =
    buildMessage(neg.id, I.accept).asInstanceOf[T]
}

