package feh.tec.agents.lite

import feh.tec.agents.lite.AgentCreationInterface.{NegotiationInit, ArgDescription, NegotiationInitExtended}
import feh.tec.agents.lite.spec.NegotiationSpecification.Interlocutors

import scala.reflect.ClassTag


trait AgentCreationInterface {
  self: AbstractAgent =>

  type Args = Map[String, Any]

  val uniqueName: String
  val role: NegotiationRole
  val negotiationsInit: Set[NegotiationInit]
  val args: Args

  lazy val name: String = uniqueName
}

case class AgentCreationInterfaceDescriptor(descriptions: Map[String, ArgDescription[_]]){
  final def argsCount = descriptions.size
}

object AgentCreationInterface{

  abstract class ArgDescription[T: ClassTag]{
    lazy val clazz = scala.reflect.classTag[T].runtimeClass.asInstanceOf[Class[T]]
  }

  case class SystemAgentRef(role: SystemRole) extends ArgDescription[AgentRef]
  case object InitialDistinctPriority extends ArgDescription[Priority]

  case class NegotiationInit(id: NegotiationId, issues: Set[Var]/*, initialPriority: Priority*/){
    def pair = id -> issues
  }
  case class NegotiationInitExtended(init: NegotiationInit, scope: Interlocutors) {
    def id = init.id
    def issues = init.issues
  }

}