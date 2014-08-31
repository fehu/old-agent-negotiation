package feh.tec.agents


/** A view on an aspect
  * helper for decision making
 */
sealed trait View {
  def aspect: String

  override def toString = s"View($aspect)"
}

/** A view on an inner aspect
  */
trait InternalView extends View

/** A view on an external aspect
  * `process` should be called for all messages incoming
  *  gathers some `Data` about interlocutors
  */
trait ExternalView extends View{
  type Data

  def data: Map[AgentRef, Data]

  def process: PartialFunction[AbstractMessage, Unit]
}

/** A view over own constraints
 *  checks if a value satisfies the constraints
 */
trait ConstraintsView extends InternalView{
  import ConstraintsView._

  val aspect = "Constraints over " + constraints.map(_.over).mkString(", ")

  def constraints: Set[Constraint[Var]]

  def satisfies(issue: Var, value: Any): Boolean
}

object ConstraintsView{
  case class Constraint[V <: Var](over: V, in: NegotiationId, satisfies: V#Tpe => Boolean)
}


/** Gathers priority from all messages
 */
trait PriorityView extends ExternalView{
  type Data = (NegotiationId, Priority)

  def aspect = "Interlocutors' priorities by negotiation"
}

/** Gathers data from proposals and counter-proposals
 */
trait InterlocutorsVarsView extends ExternalView{
  type Data = Map[Var, Any]

  def aspect = "Interlocutors' vars"
}

/** Gathers responses to proposals
 *  the look on if the current values satisfy the constraints of others
 */
trait ExternalConstraintsView extends ExternalView{
  /** Response to a proposal by id
   */
  type Data = Map[Message.Id, Option[Boolean]] // accepted / rejected / unknown

  def aspect = "Interlocutors' proposal responses"

  def discard(id: Message.Id)
}