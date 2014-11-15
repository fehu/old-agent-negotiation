package feh.tec.agents.light.spec.macros


import akka.actor.Props
import feh.tec.agents.light.AgentCreationInterface.NegotiationInit
import feh.tec.agents.light._
import feh.tec.agents.light.impl.{NegotiationEnvironmentController, agent}
import feh.tec.agents.light.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.light.spec.{NegotiationSpecification, AgentSpecification}
import feh.util._
import scala.collection.mutable
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
  with NegotiationBuildingMacro[C]
  with HasConstraintsBuilder[C]
{
  import c.universe._

  def controllerPropsExpr(dsl: c.Expr[spec.dsl.Negotiation], trees: Trees, cBuilder: ConstraintsBuilder): c.Expr[Props] = {
    val Trees(controller, _) = segments(build(dsl), cBuilder)(trees)
    c.Expr[Props](q"""${actorCreatePropsExpr(controller)}(Map())""")
  }

  def segments(negRaw: NegotiationRaw, cBuilder: ConstraintsBuilder): MacroBuildingSeq = MacroBuildingSeq(
    EmptyAgentTrees(negRaw) ::
    PriorityAndProposalBasedAgentSegment(negRaw) ::
    IteratingAllVarsAgentSegment(negRaw) ::
    RequiresDistinctPriorityAgentSegment(negRaw) ::
    ControllerParent ::
    EmbedAgentsProps(negRaw, cBuilder) ::
    ExtraArgsValues :: Nil
  )

  protected def transform(in: List[Raw.AgentDef])(f: (ActorTrees, Raw.AgentDef) => ActorTrees): I[(AgentName, ActorTrees)] =
    original => in.find(_.name == original._1).map(raw => original._1 -> f(original._2, raw)) getOrElse original

  private var requiredAgentArgs = Map.empty[String, mutable.Map[String, (c.Type, c.Tree)]].withDefault(_ => mutable.HashMap.empty[String, (c.Type, c.Tree)])

  protected def agentArgsRequired(agent: String) = requiredAgentArgs(agent).toMap
  protected def addAgentArgs(agentName: String, argName: String, argType: c.Type, argTree: c.Tree) = {
    requiredAgentArgs += agentName -> (requiredAgentArgs(agentName) += argName -> (argType, argTree))
  }

  def EmptyAgentTrees(raw: NegotiationRaw, anonAgentClassName: String = "$AgentAnonClass") = MacroSegment{
    case tr@Trees(_, ags) if ags.isEmpty =>
      val agents = raw.agents.map(_.name -> ActorTrees(anonAgentClassName, Nil, Nil, Map())).toMap
      tr.copy(agents = agents)
  }

  def PriorityAndProposalBasedAgentSegment(raw: NegotiationRaw) = raw match {
    case NegotiationRaw(vars, negs, agents, _, _) =>
      val priorityAndProposalBased = agents.filter(_.spec.actualType <:< c.typeOf[PriorityAndProposalBasedAgentSpec[_, _]])
      MacroSegment{
        case Trees(controller, ags) =>
          val newAg = ags.map{
            original =>
              priorityAndProposalBased.find(_.name == original._1)
              .map{
                case Raw.AgentDef(name, _, _, _) =>
                  name -> original._2
                    .append.parents(
                      c.typeOf[agent.PriorityAndProposalBasedAgent[Language.ProposalBased with Language.HasPriority]]
                    )
                    .add.constructorArgs(
                      "uniqueName" -> c.typeOf[String],
                      "role" -> c.typeOf[NegotiationRole],
                      "negotiationsInit" -> c.typeOf[Set[NegotiationInit]],
                      "args" -> c.typeOf[AgentCreationInterface#Args]
                    )
              }
              .getOrElse(original)
          }.toMap

          Trees(controller, newAg)
      }
  }

  def IteratingAllVarsAgentSegment(raw: NegotiationRaw) = raw match {
    case NegotiationRaw(_, _, agents, _, _) =>
      val iteratingAllVars = agents.filter(_.spec.actualType <:< c.typeOf[IteratingSpec.AllVars[_, _]])
      MacroSegment{
        case Trees(controller, ags) =>
          val newAgs = ags.map(
            transform(iteratingAllVars){
              (trees, raw) =>
                trees
                  .append.parents(
                    c.typeOf[agent.DomainIteratingAllVars[Language.ProposalBased with Language.HasPriority]]
                  )
            }
          )

          Trees(controller, newAgs)
      }
  }

  def RequiresDistinctPriorityAgentSegment(raw: NegotiationRaw) = {
    val requireDistinctPriority = raw.agents.filter(_.spec.actualType <:< c.typeOf[spec.RequiresDistinctPriority])

    val controllerExtra =q"""
      protected object InitialPriority {
        private var p = Map.empty[NegotiationId, Priority].withDefault(_ => new Priority(0))
        def next(neg: NegotiationId) = {
          p += neg -> p(neg).raise()
          p(neg)
        }
      }
    """
    def argTree(agent: String) = {
      val entries = raw.agents.find(_.name == agent).get.negotiations.map{
        case Raw.AgentNegDef(negName, _, _, _, _) =>
          val negId = q"NegotiationId($negName)"
          q"$negId -> InitialPriority.next($negId)"
      }
      q"Map(..$entries)"
    }

    MacroSegment{
      case Trees(controller, ags) =>
        val newAgs = ags.map {
          ag =>
            addAgentArgs(ag._1, "initial-priority", c.typeOf[Map[NegotiationId, Priority]], argTree(ag._1))
            transform(requireDistinctPriority) {
              (trees, raw) =>
                trees
                  .append.parents(c.typeOf[agent.RequiresDistinctPriority])
                  .append.body( q"""val initialPriority = args("initial-priority").asInstanceOf[Map[NegotiationId, Priority]]""")
            } (ag)
        }
        Trees(if(requireDistinctPriority.nonEmpty) controller.append.body(controllerExtra) else controller, newAgs)
    }
  }

  def ControllerParent = MacroSegment{
    case Trees(controller, ags) =>
      val newController = controller
        .prepend.parents(c.typeOf[NegotiationEnvironmentController])
      Trees(newController, ags)
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
              ??? //AgentRef(Agent.Id(name, propsArgs("role")), actorRef)
            }
          """
          c.Expr(q"$agentDef -> $buildTree")
      }

      val newController = controller
        .prepend.body(q"""
           import feh.tec.agents.light.spec.NegotiationSpecification._
           protected val initialAgents: List[(AgentDef, CreateInterface => AgentRef)] = List(..$initialAgents)
        """)

      Trees(newController, ags)
  }

  def ExtraArgsValues = MacroSegment{
    case trees@Trees(controller, ags) =>
      val liftedArgsByNameAndAg = ags map { case (agName, _) => agName -> q"${agentArgsRequired(agName).mapValues(p => q"() => $p")}" }

      val extraArgs = q"""
        private val liftedArgsByNameAndAg: Map[String, Map[String, () => Any]] =
          Map(..${
            liftedArgsByNameAndAg.map{ case (name, tree) => q"$name -> $tree" }
          })
        protected def extraArgs(agent: String): Map[String, Any] = liftedArgsByNameAndAg(agent).mapValues(_())
      """

      trees.copy(controller = controller.append.body(extraArgs))
  }
}
