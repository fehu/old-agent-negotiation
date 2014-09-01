package feh.tec.agents.service

import feh.tec.agents.Message.AutoId
import feh.tec.agents._
import feh.tec.agents.service.ConflictResolver.{ConflictResolved, ResolveConflict}

import scala.collection.mutable
import scala.util.Random

/** Resolves any conflict between two agents
 */
trait ConflictResolver extends AbstractAgent{
  def role = ConflictResolver.Role

  /** keys are (sender, issue)
   */
  val conflicts = mutable.HashMap.empty[(AgentRef, Any), ResolveConflict]

  def lifeCycle = {
    case req@ResolveConflict(issue, _, opponent) if conflicts contains opponent -> issue =>
      val (winnerReq, looserReq) = {
                                      val p = (req, conflicts.remove(opponent -> issue).get)
                                      if(Random.nextBoolean()) p else p.swap
                                    }
      winnerReq.sender ! ConflictResolved(winnerReq.id, won = true, winnerReq.value)
      looserReq.sender ! ConflictResolved(looserReq.id, won = false, winnerReq.value)
    case req@ResolveConflict(issue, _, opponent) =>
      conflicts += (req.sender, issue) -> req
  }
}

object ConflictResolver{
  case object Role extends SystemRole{ val name = "ConflictResolver" }

  case class ResolveConflict(issue: Any, value: Any, opponent: AgentRef)(implicit val sender: AgentRef) extends SystemMessage with AutoId

  /**
   * responds with same Id as request
   */
  case class ConflictResolved protected[ConflictResolver](requestId: Message.Id, won: Boolean, value: Any)
                                                         (implicit val sender: AgentRef)
    extends SystemMessage { def id = requestId }
}