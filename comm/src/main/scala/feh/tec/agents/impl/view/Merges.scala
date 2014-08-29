package feh.tec.agents.impl.view

import feh.tec.agents._
import feh.tec.agents

class ConstraintsSatisfactionWithPriority(constraints: ExternalConstraintsView, priority: PriorityView)
  extends ViewMerge._2(constraints, priority)({
    case pair => pair
  })

object ConstraintsSatisfactionWithPriority{
  type Data = (Map[Message.Id, Option[Boolean]], agents.Priority)

  def apply[Lang <: ProposalLanguage](lang: Lang, filter: Message => Boolean = _ => true) =
    new ConstraintsSatisfactionWithPriority(new ExternalConstraints(lang, filter), new Priority(filter))
}