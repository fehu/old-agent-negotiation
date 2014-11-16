package feh.tec.agents.light.spec.macros

import akka.actor.Props
import feh.tec.agents.light.AgentCreationInterface.NegotiationInit
import feh.tec.agents.light._
import feh.tec.agents.light.impl.{NegotiationEnvironmentController, agent}
import feh.tec.agents.light.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.light.spec.NegotiationSpecification.NegotiationDef
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

trait ControllerBuildingAgentsMacro[C <: whitebox.Context] extends ControllerBuildingMacro[C] with NegotiationBuildingMacro[C]{
  def AgentBuildSegments(negRaw: NegotiationRaw): List[MacroSegment]
  protected def agentArgsRequired(agent: String): Map[String, (c.Type, c.Tree)]
}


trait ControllerBuildingMacroImpl[C <: whitebox.Context] extends ControllerBuildingAgentsMacro[C] with HasConstraintsBuilder[C]{
  import c.universe._

  def controllerPropsExpr(dsl: c.Expr[spec.dsl.Negotiation], trees: Trees, cBuilder: ConstraintsBuilder): c.Expr[Props] = {
    val Trees(controller, _) = segments(build(dsl), cBuilder)(trees)
    c.Expr[Props](q"""${actorCreatePropsExpr(controller)}(Map())""")
  }

  def segments(negRaw: NegotiationRaw, cBuilder: ConstraintsBuilder): MacroBuildingSeq = MacroBuildingSeq(
    AgentBuildSegments(negRaw) :::
    ControllerParent ::
    EmbedIssues(negRaw) ::
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

  def EmbedIssues(raw: NegotiationRaw) = MacroSegment{
    case trees@Trees(controller, _) =>
      val issues = raw.vars.map{
        case Raw.VarDef(name, Raw.DomainDef(domain, tpe, domTpe)) =>
          val domainMix = domTpe match {
            case t if t <:< typeOf[Range]   => tq"Domain.Range"
            case t if t <:< tq"Set[$tpe]".tpe  => tq"Domain.Small[$tpe]"
          }
          q"$name -> new Var($name, _.isInstanceOf[$tpe]) with $domainMix { def domain: $domTpe = $domain }"
      }
      val issuesByNeg = raw.negotiations.map{ case NegotiationDef(name, i) => q"$name -> Seq(..$i)" }

      trees.copy(controller = controller.append.body(q"""
        protected val issues: Map[String, Var] = Map(..$issues)
        protected val issuesByNegotiation: Map[String, Seq[String]] = Map(..$issuesByNeg)
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

  def EmbedSpawnsAndTimeouts(raw: NegotiationRaw) = MacroSegment{
    case trees@Trees(controller, _) =>
      val spawns = raw.spawns.flatMap{
        case Raw.SpawnDefs(defs) => defs.map{
          case Raw.SingleSpawnDef(name, count) => q"$name -> $count"
        }
      }
      val timeouts = raw.time.flatMap(_.mp).toMap

      trees.copy(controller = controller.append.body(
        q"protected val spawns: Map[String, Int] = Map(..$spawns)",
        q"""
            import feh.tec.agents.light.impl.NegotiationEnvironmentController._
            protected val timeouts: Timeouts = new Timeouts {
              lazy val initialize = ${timeouts.getOrElse("initialize", c.Expr(q"DefaultTimeouts.initialize"))}
              lazy val start = ${timeouts.getOrElse("start", c.Expr(q"DefaultTimeouts.start"))}
              lazy val stop = ${timeouts.getOrElse("stop", c.Expr(q"DefaultTimeouts.stop"))}
              lazy val reset = ${timeouts.getOrElse("reset", c.Expr(q"DefaultTimeouts.reset"))}
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


trait ControllerBuildingAgentsMacroImpl[C <: whitebox.Context] extends ControllerBuildingAgentsMacro[C]{
  import c.universe._

  def AgentBuildSegments(negRaw: NegotiationRaw) =
    EmptyAgentTrees(negRaw) ::
    PriorityAndProposalBasedAgentSegment(negRaw) ::
    IteratingAllVarsAgentSegment(negRaw) ::
    RequiresDistinctPriorityAgentSegment(negRaw) ::
    ReportingAgent(negRaw) ::
    TypesDefinitions :: Nil


  protected def transform(in: List[Raw.AgentDef])(f: (ActorTrees, Raw.AgentDef) => ActorTrees): I[(AgentName, ActorTrees)] =
    original => in.find(_.name == original._1).map(raw => original._1 -> f(original._2, raw)) getOrElse original

  private var requiredAgentArgs = Map.empty[String, mutable.Map[String, (c.Type, c.Tree)]].withDefault(_ => mutable.HashMap.empty[String, (c.Type, c.Tree)])

  protected def agentArgsRequired(agent: String) = requiredAgentArgs(agent).toMap

  protected def addAgentArgs(agentName: String, argName: String, argType: c.Type, argTree: c.Tree) = {
    requiredAgentArgs += agentName -> (requiredAgentArgs(agentName) += argName ->(argType, argTree))
  }

  def EmptyAgentTrees(raw: NegotiationRaw, anonAgentClassName: String = "$AgentAnonClass") = MacroSegment {
    case tr@Trees(_, ags) if ags.isEmpty =>
      val agents = raw.agents.map(_.name -> ActorTrees(anonAgentClassName, Nil, Nil, Map())).toMap
      tr.copy(agents = agents)
  }

  def PriorityAndProposalBasedAgentSegment(raw: NegotiationRaw) = raw match {
    case NegotiationRaw(vars, negs, agents, _, _) =>
      val priorityAndProposalBased = agents.filter(_.spec.actualType <:< c.typeOf[PriorityAndProposalBasedAgentSpec[_, _]])
      MacroSegment {
        case Trees(controller, ags) =>
          val newAg = ags.map {
            original =>
              priorityAndProposalBased.find(_.name == original._1)
                .map {
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
      MacroSegment {
        case Trees(controller, ags) =>
          val newAgs = ags.map(
            transform(iteratingAllVars) {
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

    val controllerExtra = q"""
      protected object InitialPriority {
        private var p = Map.empty[NegotiationId, Priority].withDefault(_ => new Priority(0))
        def next(neg: NegotiationId) = {
          p += neg -> p(neg).raise()
          p(neg)
        }
      }
    """
    def argTree(agent: String) = {
      val entries = raw.agents.find(_.name == agent).get.negotiations.map {
        case Raw.AgentNegDef(negName, _, _, _, _) =>
          val negId = q"NegotiationId($negName)"
          q"$negId -> InitialPriority.next($negId)"
      }
      q"Map(..$entries)"
    }

    MacroSegment {
      case Trees(controller, ags) =>
        val newAgs = ags.map {
          ag =>
            addAgentArgs(ag._1, "initial-priority", c.typeOf[Map[NegotiationId, Priority]], argTree(ag._1))
            transform(requireDistinctPriority) {
              (trees, raw) =>
                trees
                  .append.parents(c.typeOf[agent.RequiresDistinctPriority])
                  .append.body( q"""val initialPriority = args("initial-priority").asInstanceOf[Map[NegotiationId, Priority]]""")
            }(ag)
        }
        Trees(if (requireDistinctPriority.nonEmpty) controller.append.body(controllerExtra) else controller, newAgs)
    }
  }

  protected def agentLang(trees: ActorTrees): c.Type = trees.parents
    .flatMap(_.typeArgs.filter(_ <:< typeOf[NegotiationLanguage]))
    .sortWith(_ <:< _).lastOption getOrThrow s"no language type parameter found in $trees"


  def ReportingAgent(raw: NegotiationRaw) = {
    val reportingAgents = raw.agents.filter(_.negotiations.exists(_.reportingToOpt.isDefined))

    def agentParent(agTrees: ActorTrees): c.Type = typeOf[AutoReporting[NegotiationLanguage]] match {
      case TypeRef(pre, sym, args) => internal.typeRef(pre, sym, agentLang(agTrees) :: Nil)
    }

    def reportListenerRoleTree = q"feh.tec.agents.light.impl.service.DefaultReportWriter.Role"
    def reportToTree = q"""
      systemAgents.find(_.id.role == $reportListenerRoleTree)
        .getOrElse(sys.error("no system agent for with " + $reportListenerRoleTree + " found"))
    """

    def reportSysAgent = "report-listener" -> ActorTrees("$ReportListener",
      parents = typeOf[impl.service.DefaultReportWriter] :: Nil,
      body = Nil,
      constructorArgs = Map("writeTo" -> typeOf[java.io.File])
    )
    def addReportSysAgent() = {
      addAgentArgs("report-listener", "writeTo", typeOf[java.io.File], Ident(TermName("reportFile")) )
      addAgentArgs("report-listener", "role", typeOf[Role], reportListenerRoleTree)
    }

    MacroSegment{
      case Trees(controller, ags) =>
        val newAgs = ags map {
          case (name, trees) if reportingAgents.exists(_.name == name) =>
            val reportingTo = reportingAgents.find(_.name == name).get.negotiations.map{
              case Raw.AgentNegDef(neg, _, _, Some(_), _) => q"""NegotiationId($neg) -> args("report-to").asInstanceOf[AgentRef]"""
            }

            val tr = trees
              .append.parents(agentParent(trees))
              .append.body(q"val reportingTo: Map[NegotiationId, AgentRef] = Map(..$reportingTo)")
              .add.constructorArgs("report-to" -> typeOf[AgentRef])
            addAgentArgs(name, "report-to", typeOf[AgentRef], reportToTree)
            name -> tr
          case p => p
        }

        val sys = if(reportingAgents.nonEmpty) {
          addReportSysAgent()
          reportSysAgent :: Nil
        } else Nil

        val newController = controller.append.parents(typeOf[impl.service.ReportPrinterSupportBundle])

        Trees(newController, newAgs ++ sys)
    }
  }

  def TypesDefinitions = MacroSegment{
    case trees@Trees(_, ags) =>

      val newAgs = ags.map{
        case (name, tr@ActorTrees(_, parents, _, _)) if !parents.exists(_ <:< typeOf[SystemAgent]) =>
          val abstractTypeMembers = parents
            .flatMap(
              _.members
                .withFilter(d => d.name.isTypeName && d.isAbstract)
                .map(
                  sym =>
                    sym.asType.name ->
                      (sym.typeSignature match {
                        case TypeBounds(_, RefinedType(tpes, _)) => tpes
                        case TypeBounds(_, ref: TypeRef) => ref :: Nil
                        case _ => Nil
                      })
                )
            ).groupBy(_._1)
            .mapValues(_.unzip._2.flatten.distinct).filter(_._2.nonEmpty)

          val langTpe = agentLang(tr)
//          c.abort(NoPosition, "##!!M " + showRaw(parents.zipMap(p => scala.util.Try{p.member(TypeName("Negotiation")).typeSignature})))
          val typeDefs = abstractTypeMembers
            .mapValues(_.map{
              case TypeRef(pre, sym, args) =>
                val newArgs = args.map{
                  case lang if lang <:< typeOf[Language] => langTpe
                  case other => other
                }
                internal.typeRef(pre, sym, newArgs)
              case t => c.abort(NoPosition, "##!! " + showRaw(t))
//              tpe => tpe
//                .typeArgs.find(_ <:< typeOf[Language])
//                .map{
//                  case TypeRef(_, tName, )
////                  t => c.abort(NoPosition, "##!! + " + showRaw(t))
//                }
//                .getOrElse(tpe)
              })
            .toList.map{ case (tName, ext :: mix) => q"type $tName = $ext with ..$mix" }

//          c.abort(NoPosition, "##!!2 " + showRaw(typeDefs))
          name -> tr.prepend.body(typeDefs: _*)
        case system => system
      }

      trees.copy(agents = newAgs)
  }

}
