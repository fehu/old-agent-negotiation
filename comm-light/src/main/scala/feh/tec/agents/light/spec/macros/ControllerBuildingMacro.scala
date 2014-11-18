package feh.tec.agents.light.spec.macros

import akka.actor.Props
import feh.tec.agents.light.AgentCreationInterface.NegotiationInit
import feh.tec.agents.light._
import feh.tec.agents.light.impl.{NegotiationEnvironmentController, agent}
import feh.tec.agents.light.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.light.spec.NegotiationSpecification.NegotiationDef
import feh.tec.agents.light.spec.{NegotiationSpecification, AgentSpecification}
import feh.util._
import scala.collection.immutable.HashSet
import scala.collection.{IterableLike, mutable}
import scala.reflect.macros.whitebox

trait ControllerBuildingMacro[C <: whitebox.Context] extends ActorBuildingMacro[C] {
  type AgentName = String
  case class Trees(controller: ActorTrees, agents: Map[AgentName, ActorTrees])
  object Trees{ def empty(name: String) = Trees(ActorTrees(name, Nil, Nil, Map()), Map()) }
  type MacroSegment = Trees => Trees

  def MacroSegment(transform: PartialFunction[Trees, Trees]): MacroSegment = {
    trees => transform.lift(trees).getOrElse(trees)
  }

   case class MacroBuildingSeq(segments: List[MacroSegment]){
    def apply(trees: Trees) = Function.chain(segments)(trees)
  }
}

trait ControllerBuildingMacroImpl[C <: whitebox.Context] extends ControllerBuildingMacro[C]
  with AgentsBuildingMacro[C]
  with HasConstraintsBuilder[C]
{
  import c.universe._

  def controllerPropsExpr(dsl: c.Expr[spec.dsl.Negotiation], trees: Trees, cBuilder: ConstraintsBuilder): c.Expr[Props] = {
    val Trees(controller, _) = segments(build(dsl), cBuilder)(trees)
    c.Expr[Props](q"""${actorCreatePropsExpr(controller)}(Map())""")
  }

  def segments(negRaw: NegotiationRaw, cBuilder: ConstraintsBuilder): MacroBuildingSeq = MacroBuildingSeq(
    AgentBuildSegments(negRaw) :::
    ControllerParent ::
    EmbedIssuesAndDomainIteratorsCreators(negRaw) ::
    EmbedAgentsProps(negRaw, cBuilder) ::
    ExtraArgsValues ::
    EmbedSpawnsAndTimeouts(negRaw) ::
    SupportBundle :: Nil
  )

  def ControllerParent = MacroSegment{
    case Trees(controller, ags) =>
      val newController = controller
        .prepend.parents(c.typeOf[NegotiationEnvironmentController])
      Trees(newController, ags)
  }

  def EmbedIssuesAndDomainIteratorsCreators(raw: NegotiationRaw) = MacroSegment{
    case trees@Trees(controller, _) =>
      val (issues, domainIteratorsCreators) = raw.vars.map{
        case Raw.VarDef(name, Raw.DomainDef(domain, tpe, domTpe)) =>
          val (domainMix, iteratorBuilder) = domTpe match {
            case t if t <:< typeOf[Range]     => tq"Domain.Range"       -> tq"DomainIteratorBuilder.Range"
            case t if t <:< tq"Set[$tpe]".tpe => tq"Domain.Small[$tpe]" -> tq"DomainIteratorBuilder.Generic"
          }
          val issue = q"$name -> new Var($name, _.isInstanceOf[$tpe]) with $domainMix { def domain: $domTpe = $domain }"
          val domainIteratorsCreator = q"$name -> (new $iteratorBuilder).asInstanceOf[DomainIteratorBuilder[Var#Domain, Var#Tpe]]"

          issue -> domainIteratorsCreator
      }.unzip
      val issuesByNeg = raw.negotiations.map{ case NegotiationDef(name, i) => q"$name -> Seq(..$i)" }

      trees.copy(controller = controller.append.body(q"""
        protected val issues: Map[String, Var] = Map(..$issues)
        protected val issuesByNegotiation: Map[String, Seq[String]] = Map(..$issuesByNeg)
        protected val domainIteratorsCreators: Map[String, DomainIteratorBuilder[Var#Domain, Var#Tpe]] = Map(..$domainIteratorsCreators)
      """
      ))
  }

  def EmbedAgentsProps(raw: NegotiationRaw, cBuilder: ConstraintsBuilder) = MacroSegment{
    case Trees(controller, ags) =>
      val rawNames = raw.agents.map(_.name)
      val (agents, sysAgents) = ags.partition(rawNames contains _._1)

      val initialAgents: List[c.Expr[(NegotiationSpecification.AgentDef, NegotiationEnvironmentController#CreateInterface => AgentRef)]] = raw.agents map{
        case Raw.AgentDef(name, role, negs, spec) =>
          val negotiations = negs map {
            case Raw.AgentNegDef(negName, scope, scopeExpr, reportToExprOpt, constraints) =>
              val constraintsTrees = constraints map (cBuilder.build(_, raw)) map (_.tree)
              val extraTrees = constraintsTrees
              q"feh.tec.agents.light.spec.NegotiationSpecification.AgentNegDef($negName, $scopeExpr, Seq(..$extraTrees))"
          }
          val agentDef = q"""
            feh.tec.agents.light.spec.NegotiationSpecification
              .AgentDef($name, $role, Seq(..$negotiations), $spec)"""

          val agentTrees = agents(name)
          val actorProps = actorCreatePropsExpr(agentTrees)

          val buildTree = q"""
            (propsArgs: Map[String, Any]) => {
              val name = propsArgs("uniqueName").asInstanceOf[String]
              val props = $actorProps(propsArgs)
              val actorRef = implicitly[ActorSystem].actorOf(props, name)
              AgentRef(Agent.Id(name, propsArgs("role").asInstanceOf[Role]), actorRef)
            }
          """
          c.Expr(q"$agentDef -> $buildTree")
      }

      val systemAgentsInit: List[c.Expr[() => AgentRef]] = sysAgents.toList.map{
        case (name, trees) =>
          val ci = actorCreatePropsExpr(trees)
          c.Expr[() => AgentRef](q"""
            val args = extraArgs($name)
            def props = $ci(args)
            def role = args("role").asInstanceOf[Role]
            def ref = implicitly[ActorSystem].actorOf(props, $name)
            () => AgentRef(Agent.Id($name, role), ref)
          """)
      }

      val newController = controller
        .append.body(q"""
           import feh.tec.agents.light.spec.NegotiationSpecification._
           protected val initialAgents: List[(AgentDef, CreateInterface => AgentRef)] = List(..$initialAgents)
           protected val systemAgentsInit: Set[() => AgentRef] = Set(..$systemAgentsInit)
        """)

      Trees(newController, ags)
  }

  def ExtraArgsValues = MacroSegment{
    case trees@Trees(controller, ags) =>
      val liftedArgsByNameAndAg = ags map { case (agName, _) => agName -> q"${agentArgsRequired(agName).mapValues(p => q"() => ${p._2}")}" }

      val extraArgs = q"""
        log.debug("initialAgents = " + initialAgents)
        private lazy val liftedArgsByNameAndAg: Map[String, Map[String, () => Any]] =
          Map(..${
            liftedArgsByNameAndAg.map{ case (name, tree) => q"$name -> $tree" }
          })
        protected def extraArgs(agent: String): Map[String, Any] = liftedArgsByNameAndAg(agent).map{
          case (n, f) =>
            val v = f()
            log.debug("arg(" + n + ")=" + v)
            n -> v
        }
      """

      trees.copy(controller = controller.append.body(extraArgs))
  }

  def EmbedSpawnsAndTimeouts(raw: NegotiationRaw) = MacroSegment{
    case trees@Trees(controller, _) =>
      val spawns = raw.spawns.flatMap{
        case Raw.SpawnDefs(defs) => defs.map{
          case Raw.SingleSpawnDef(name, count) => q"$name -> $count"
        }
      }
      val timeouts = raw.time.flatMap(_.mp.mapValues(dur => q"akka.util.Timeout($dur)")).toMap

      trees.copy(controller = controller.append.body(
        q"protected val spawns: Map[String, Int] = Map(..$spawns)",
        q"""
            import feh.tec.agents.light.impl.NegotiationEnvironmentController._
            protected val timeouts: Timeouts = new Timeouts {
              lazy val initialize = ${timeouts.getOrElse("initialize", q"DefaultTimeouts.initialize")}
              lazy val start = ${timeouts.getOrElse("start", q"DefaultTimeouts.start")}
              lazy val stop = ${timeouts.getOrElse("stop", q"DefaultTimeouts.stop")}
              lazy val reset = ${timeouts.getOrElse("reset", q"DefaultTimeouts.reset")}
         }"""
      ))
  }

  def SupportBundle = MacroSegment{
    case trees@Trees(controller, _) =>
      val newController = if(controller.parents.exists(_ <:< typeOf[impl.service.SupportBundle]))
        controller.prepend.body(
          q"def configInfo = feh.tec.agents.light.impl.service.SupportBundle.Config(${Configs.controller[c.type](c)})"
        )
      else controller

      trees.copy(controller = newController)
  }
}


