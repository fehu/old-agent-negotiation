package feh.tec.agents.impl

import akka.actor.ActorLogging
import feh.tec.agents.SystemMessage.{Start, ScopeUpdate}
import feh.tec.agents.impl.ProposalEngine.SharingKnowledge
import feh.tec.agents.{SystemMessage, SystemRole, AgentRef, Role}

import scala.collection.mutable

object KnowledgeSharing{
  type Service = KnowledgeSharing with System.Service.Args0

  object Role extends SystemRole{ val name: String = "KnowledgeShare" }
}

class KnowledgeSharing extends SystemAgent with ActorLogging{
  def role = KnowledgeSharing.Role

  val scopes = mutable.HashMap.empty[Role, Set[AgentRef]].withDefaultValue(Set())

  override def processSys = super.processSys orElse{
    case Start() =>
    case ScopeUpdate.NewScope(scp, _) => scp.groupBy(_.id.role) foreach{
      case (role, refs) => scopes(role) = scopes(role) ++ refs
    }
    case msg: SharingKnowledge.ProvenFailure => resendToScope(msg)
  }

  private def resendToScope(msg: SharingKnowledge.ProvenFailure) =
    scopes get msg.senderId.role foreach (_ withFilter (_.id != msg.senderId) foreach (_.ref ! msg))
}
