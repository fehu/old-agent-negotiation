package feh.tec.agents.lite.spec.macros.exp.impl.controller

import feh.tec.agents.lite.AgentRef
import feh.tec.agents.lite.impl.NegotiationEnvironmentController
import feh.tec.agents.lite.spec.NegotiationSpecification
import feh.tec.agents.lite.spec.macros.HasConstraintsBuilder
import feh.tec.agents.lite.spec.macros.exp.ControllerBuildingMacroExperimentalBase
import scala.reflect.macros.whitebox

/** Contains `MacroSegmentsTransform`s for **EmbedAgentProps**
  */
trait EmbedAgentProps[C <: whitebox.Context]{
  self: ControllerBuildingMacroExperimentalBase[C] with HasConstraintsBuilder[C] =>

  def allEmbedAgentsProps(raw: NegotiationRaw, cBuilder: ConstraintsBuilder) = ControllerSegmentEmbedAgentsProps(raw, cBuilder) :: Nil

  def ControllerSegmentEmbedAgentsProps(raw: NegotiationRaw, cBuilder: ConstraintsBuilder) = MacroSegmentsTransform(
    seg => seg.append(ControllerBuildingStages.EmbedAgentProps,
      MacroSegment{
        case Trees(controller, ags) =>
          import c.universe._

          val rawNames = raw.agents.map(_.name)
          val (agents, sysAgents) = ags.partition(rawNames contains _._1)

          val initialAgents: List[c.Expr[(NegotiationSpecification.AgentDef, NegotiationEnvironmentController#CreateInterface => AgentRef)]] = raw.agents map{
            case Raw.AgentDef(name, role, negs, spec) =>
              val negotiations = negs map {
                case Raw.AgentNegDef(negName, scope, scopeExpr, reportToExprOpt, constraints) =>
                  val constraintsTrees = constraints map (cBuilder.build(_, raw)) map (_.tree)
                  val extraTrees = constraintsTrees
                  q"feh.tec.agents.lite.spec.NegotiationSpecification.AgentNegDef($negName, $scopeExpr, Seq(..$extraTrees))"
              }
              val agentDef = q"""
                feh.tec.agents.lite.spec.NegotiationSpecification
                  .AgentDef($name, $role, Seq(..$negotiations), $spec)"""

              val agentTrees = agents(name)
              val actorProps = actorCreatePropsExpr(agentTrees)

              /*
              val name = propsArgs("uniqueName").asInstanceOf[String]
              val props = $actorProps(propsArgs)
              val actorRef = implicitly[ActorSystem].actorOf(props, name)
              AgentRef(Agent.Id(name, propsArgs("role").asInstanceOf[Role]), actorRef)
               */
              val buildTree = q"""
                (propsArgs: Map[String, Any]) => {
                  ???
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
               import feh.tec.agents.lite.spec.NegotiationSpecification._
               protected val initialAgents: List[(AgentDef, CreateInterface => AgentRef)] = List(..$initialAgents)
               protected val systemAgentsInit: Set[() => AgentRef] = Set(..$systemAgentsInit)
            """)

          Trees(newController, ags)
      }

    )
  )

}
