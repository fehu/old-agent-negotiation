package feh.tec.agents.light.spec

//import akka.actor.{Props, ActorSystem}
//import feh.tec.agents.light.spec.AgentProps.{NegotiationInitExtended, NegotiationInit}
//import feh.tec.agents.light.spec.NegotiationSpecification.Interlocutors
//import feh.tec.agents.light._

/*
case class AgentProps[Arg](name: String, role: NegotiationRole, negotiationInits: Set[NegotiationInitExtended],
                           build: AgentProps.Building[Arg])
{
  lazy val id = Agent.Id(name, role)
  def props(arg: Arg) = build(name, role, negotiationInits.map(_.init), arg)
  def create(arg: Arg)(implicit sys: ActorSystem) = AgentRef(id, sys.actorOf(props(arg), name))
}
*/

/*
object AgentProps{
  case class Building[Args](argsDescription: ArgsDescription, buildProps: (/*name: */String, NegotiationRole, Set[NegotiationInit], Args) => Props){
    def apply(name: String, role: NegotiationRole, negInit: Set[NegotiationInit], args: Args) = buildProps(name, role, negInit, args)
  }
  case class NegotiationInit(id: NegotiationId, issues: Set[Var])
  case class NegotiationInitExtended(init: NegotiationInit, scope: Interlocutors) {
    def id = init.id
    def issues = init.issues
  }
  trait CanProvide[Arg]{
    def get: Arg
  }


  case object NoArgs extends ArgsDescription{
    def isDefined = false
    def product = None
    def sequence = None
  }
  
  case class AgentPropsBundle[Arg](props: AgentProps[Arg], provider: CanProvide[Arg]){
    def props()(implicit sys: ActorSystem): Props = props.props(provider.get)
    def create()(implicit sys: ActorSystem) = props.create(provider.get)
  }
}*/
