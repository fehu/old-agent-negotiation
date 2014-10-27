package feh.tec.agents.light.spec.macros

/*
import feh.tec.agents.light.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.light.spec.NegotiationSpecification._
import feh.tec.agents.light._
import feh.tec.agents.light.spec.macros.AgentPropsBundleBuilderOld.{DependencyBuild, Dependency, DependsOnSystemAgent}
import feh.tec.agents.light.spec.macros.NegotiationSpecificationBuilder.Raw
import feh.tec.agents.light.spec.macros.NegotiationSpecificationBuilder.Raw.TreesBuilder.ConstraintsBuilder
import scala.reflect.macros.whitebox
import feh.util._

object NegotiationControllerBuilderOld {
  def build(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[EnvironmentController] = {
    import c.universe._

    implicit val cb: ConstraintsBuilder = ???

    val ExtractRaw = NegotiationSpecificationBuilder.Raw.BuildTrees[c.type](c)
    val raw = NegotiationSpecificationBuilder.raw[c.type](c)(dsl)
//    val ExtractRaw(variablesTree, negotiationsTree, agentsTree, spawnsTree, timingsTree, timeoutsTree) = raw

    val bb = new AgentPropsBundleBuilderOld[c.type](c)

    val agentDefs = raw.agents.map{ case ad: Raw.AgentDef[c.type] => ad.name -> ad }.toMap
    val negotiationDefs = raw.negotiations.map{ case nd: NegotiationDef => nd.name -> nd }.toMap
    val variableDefs = raw.variables.map{ case vd: Raw.VarDef[c.type] => vd.name -> vd }.toMap

    val buildPropsAndExprWithDependencies = raw.spawns match{
      case Raw.SpawnDefs(spwns) => spwns.map{
        case Raw.SingleSpawnDef(agName, countExpr) =>
          val ag = agentDefs(agName)
          ((
            (c: Int) => bb.props(agName+"-"+c, ag, negotiationDefs)

          ) -> countExpr) -> bb.dependencies(ag)
        }
      }

    def agPropsBundleByCount =
      buildPropsAndExprWithDependencies map {
        case ((buildPropsExpr, countExpr), dependencies) =>
          val (calls, deps) = (dependencies map dependencyCall[Any]).unzip
          c.Expr[Int => AgentPropsBundle[_]](q"""
            (c: Int) =>
              AgentPropsBundle(
                $buildPropsExpr(c),
                new AgentProps.CanProvide{ def get = Seq(..$calls) }
              )
          """) -> deps.flatten.toSeq --> countExpr
      }

    def dependencyCall[T](dep: Dependency): (c.Expr[T], Seq[Dependency])

    agDependencies.flatten.distinct map {
      case DependsOnSystemAgent(role) => bb.sysAgentForRole(role)
    }

    val propBundles = ???

    val tr =
      q"""
        import feh.tec.agents.light.impl.NegotiationEnvironmentController
        import akka.util.Timeout

        new NegotiationEnvironmentController{
          protected lazy val initialAgents = Set(..${props.flatten})
          protected lazy val systemAgentsProps = ???
          protected lazy val timeouts = new NegotiationEnvironmentController.Timeouts {
            lazy val initialize: Timeout = ???
            lazy val start: Timeout = ???
            lazy val stop: Timeout = ???
            lazy val reset: Timeout = ???
          }
        }
       """
    c.Expr(q"println(${showRaw(tr)}); $tr")
  }
}

object AgentPropsBundleBuilderOld{
  trait Dependency
  case class DependsOnSystemAgent(role: SystemRole)

  case class DependencyBuild[C <: whitebox.Context, T](dep: Dependency, bundle: AgentPropsBundle[T], call: C#Expr[T])
}

class AgentPropsBundleBuilderOld[C <: whitebox.Context](val c: C){
  import c.universe._

  def dependencies(definition: Raw.AgentDef[c.type]): Seq[AgentPropsBundleBuilderOld.Dependency] = ???

  def props(
             uniqueName: String,
             definition: Raw.AgentDef[c.type],
             negotiationByName: String => NegotiationDef

           ): c.Expr[AgentProps[_]] ={
    object spec{
      var PriorityAndProposalBased = false
      var iterating: Iterating.Value = Iterating.None

      object Iterating extends Enumeration{ val All, CurrentIssues, None = Value }
    }
    val specTree = definition.spec.tree

    def typeCheck[T : c.TypeTag] = c.typecheck(specTree, c.TERMmode, c.typeOf[T], silent = true).nonEmpty
    def typeCheckAndSet[T : c.TypeTag](set: => Unit) = if(typeCheck[T]) set

    typeCheckAndSet[PriorityAndProposalBasedAgentSpec[_, _]]{ spec.PriorityAndProposalBased = true }
    typeCheckAndSet[IteratingSpec.AllVars[_, _]]{ spec.iterating = spec.Iterating.All }

    val agPropsBuild =
      if(spec.PriorityAndProposalBased && spec.iterating == spec.Iterating.All){
        q"""
          impl.agent.create.PriorityAndProposalBasedIteratingAllVars(
            definition.spec.asInstanceOf[PriorityAndProposalBasedAgentSpec[PPI.Ag, PPI.Lang] with IteratingSpec.AllVars[PPI.Ag, PPI.Lang]]
           )
         """
      }
      else c.abort(c.enclosingPosition, "failed to build controller")

    val negInits = definition.negotiations.toSet.map( {
      case nd: Raw.AgentNegDef[c.type] =>
        val agNeg = definition.negotiations.find(_.negotiation == nd.negotiation).get
        negotiationInitExtended(nd.negotiation, agNeg, negotiationByName)
      }: (Raw.AgentNegDef[c.type] => c.Tree)
    )

    val propsTree = q"""
      AgentProps(
        $uniqueName,
        NegotiationRole(${definition.role}),
        Set(..$negInits),
        $agPropsBuild
      )
      """
    c.Expr(propsTree)
  }

  protected def negotiationInitExtended= {
    (negName: String, raw: Raw.AgentNegDef[c.type], negotiationByName: String => NegotiationDef) =>
      q"""
        NegotiationInitExtended(
          NegotiationInit(
            NegotiationId($negName),
            ${negotiationByName(raw.negotiation).issues.toSet}
          ),
          ${raw.interlocutorsExpr.tree.asInstanceOf[c.Tree]}
        )
      """
  }

  def sysAgentForRole(role: SystemRole): (c.Expr[Seq[Any] => AgentPropsBundle[Seq[Any]]], Seq[Dependency]) = role match {
    case smth => ???
  }
}
*/