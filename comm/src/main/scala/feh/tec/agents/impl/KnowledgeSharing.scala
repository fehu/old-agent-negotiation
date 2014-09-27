package feh.tec.agents.impl

import akka.actor.ActorLogging
import feh.tec.agents.SystemMessage.{Start, ScopeUpdate}
import feh.tec.agents.impl.ProposalEngine.SharingKnowledge.ConfigurationProvenFailure
import feh.tec.agents.{SystemRole, AgentRef, Role}

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
    case msg@ConfigurationProvenFailure(neg, conf, senderId) =>
      scopes get senderId.role foreach (_ withFilter (_.id != senderId) foreach (_.ref ! msg))
  }
}
