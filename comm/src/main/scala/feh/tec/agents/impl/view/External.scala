package feh.tec.agents.impl.view

import feh.tec.agents.Message.Id
import feh.tec.agents._

import scala.collection.mutable


trait ExternalViewImpl extends ExternalView{
  protected lazy val _data = mutable.HashMap.empty[AgentRef, Data]
  def data = _data.toMap
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
  protected lazy val _data = 
    mutable.HashMap.empty[AgentRef, mutable.Map[Message.Id, Option[Boolean]]]
      .withDefaultValue(mutable.HashMap.empty.withDefaultValue(None))
  
  def process = {
    case msg if lang.isAcceptance(msg) => _data(msg.sender) += msg.id -> Some(true)
    case msg if lang.isRejection(msg) => _data(msg.sender) += msg.id -> Some(false)
  }

  def discard(id: Id) = _data.foreach(_._2.remove(id))
}