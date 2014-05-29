package feh.agents.comm

import feh.agents.comm.Message.MessageId
import scala.util.Try

trait EnvironmentOverseer[Env] extends Agent with SystemRole{

}

object EnvironmentOverseer extends SystemRoleCompanion{
  trait EnvDSL{
    type Query <: {
      type Result
    }
    type Update
  }
  
  case class Query[DSL <: EnvDSL](query: DSL#Query)(implicit val sender: AgentId, dsl: DSL) extends Request
  case class Update[DSL <: EnvDSL](update: DSL#Update)(implicit val sender: AgentId, dsl: DSL) extends Request

  case class QueryResult[DSL <: EnvDSL, Q <: DSL#Query](result: Try[Q#Result], requestId: MessageId) extends Response
  case class UpdateResult(result: Try[Unit], requestId: MessageId) extends Response
}