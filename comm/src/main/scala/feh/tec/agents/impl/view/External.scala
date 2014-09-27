package feh.tec.agents.impl.view

import java.util.UUID

import feh.tec.agents.Message.Id
import feh.tec.agents._
import feh.util._
import scala.collection.mutable


trait ExternalViewImpl extends ExternalView{
  protected lazy val _data = mutable.HashMap.empty[AgentRef, Data]
  def data = _data.toMap

  def reset() = _data.clear()
}

/** Gathers priority from all messages */
class Priority(filter: Message => Boolean = _ => true) extends PriorityView with ExternalViewImpl{
  def process = {
    case msg: Message if filter(msg) => _data += msg.sender -> (msg.negotiation -> msg.priority)
    case _ => // ignore
  }
}

/** Gathers data from proposals and counter-proposals */
class InterlocutorsVars[Lang <: ProposalLanguage](lang: Lang,
                                                  filter: Message => Boolean = _ => true)
                                                 (implicit extractor: IssuesExtractor[Lang])
  extends InterlocutorsVarsView with ExternalViewImpl
{
  private def isCounter = lang.isInstanceOf[CounterProposalLanguage]

  def process = {
    case msg if lang.isProposal(msg) => register(msg.asInstanceOf[Message])
//    case msg if isCounter && lang.asInstanceOf[CounterProposalLanguage].isCounterProposal(msg) =>
  }

  protected def register(msg: Message) = _data += msg.sender -> extractor.extract(msg)
}

/** Gathers responses to proposals */
class ExternalConstraints[Lang <: ProposalLanguage](lang: Lang,
                                                    filter: Message => Boolean = _ => true)
  extends ExternalConstraintsView
{
  def data = _data.mapValues(_.toMap).toMap
  protected lazy val _data = mutable.HashMap.empty[AgentRef, mutable.Map[Message.Id, Option[Boolean]]]
  
  def process = {
    case msg if lang.isAcceptance(msg) => addData(msg.sender, msg.asInstanceOf[Lang#Accepted].respondingTo, Some(true))
    case msg if lang.isRejection(msg) => addData(msg.sender, msg.asInstanceOf[Lang#Accepted].respondingTo, Some(false))
    case msg if lang.isMessage(msg) && ! _data.contains(msg.sender) => //addData(msg.sender, msg.id, None)
  }

  private def addData(sender: AgentRef, id: UUID, v: Option[Boolean]) =
    _data.getOrElse(sender, mutable.HashMap.empty[Message.Id, Option[Boolean]] //.withDefaultValue(None)
      .$$(_data += sender -> _)) += id -> v

  def discard(id: Id) = _data.foreach(_._2.remove(id))
  def reset() = _data.clear()
}