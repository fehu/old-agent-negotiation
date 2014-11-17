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

trait AgentsBuildingMacro[C <: whitebox.Context] extends ControllerBuildingMacro[C] with NegotiationBuildingMacro[C]{
  def AgentBuildSegments(negRaw: NegotiationRaw): List[MacroSegment]
  protected def agentArgsRequired(agent: String): Map[String, (c.Type, c.Tree)]
}


trait ControllerBuildingMacroImpl[C <: whitebox.Context] extends AgentsBuildingMacro[C] with HasConstraintsBuilder[C]{
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


trait AgentsBuildingMacroImpl[C <: whitebox.Context] extends AgentsBuildingMacro[C]{
  import c.universe._

  def AgentBuildSegments(raw: NegotiationRaw) =
    EmptyAgentTrees(raw) ::
    PriorityAndProposalBasedAgentSegment(raw) ::
    IteratingAllVarsAgentSegment(raw) ::
    RequiresDistinctPriorityAgentSegment(raw) ::
    ReportingAgentSegment(raw) ::
    TypesDefinitionsAgentSegment ::
    CreateNegotiationAgentSegment ::
    DomainIteratorsAgentSegment ::
    ConstraintsByNegotiationAgentSegment ::
    SpecAgentSegment(raw) :: Nil


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
        private var p = Map.empty[NegotiationId, Priority]
        def next(neg: NegotiationId) = synchronized{
          val pr = p.getOrElse(neg, new Priority(0)).raise()
          p += neg -> pr
          log.debug("InitialPriority.next called for negotiation " + neg + " priority given = " + pr)
          pr
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
                  .append.body(
                    q"""lazy val initialPriority = args("initial-priority").asInstanceOf[Map[NegotiationId, Priority]]"""
                  )
            }(ag)
        }
        Trees(if (requireDistinctPriority.nonEmpty) controller.append.body(controllerExtra) else controller, newAgs)
    }
  }

  protected def agentLang(trees: ActorTrees): c.Type = trees.parents
    .flatMap(_.typeArgs.filter(_ <:< typeOf[NegotiationLanguage]))
    .sortWith(_ <:< _).lastOption getOrThrow s"no language type parameter found in $trees"


  def ReportingAgentSegment(raw: NegotiationRaw) = {
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

  def replaceTypeArg(in: c.Type, ofType: c.Type, replacement: c.Type) = in map{
    case TypeRef(pre, sym, args) =>
      val newArgs = args.map{
        case tpe if tpe <:< ofType => replacement
        case other => other
      }
      internal.typeRef(pre, sym, newArgs)
    case other => other
  }

  def replaceLanguageTypeArg(in: c.Type, forAgent: ActorTrees) = {
    val langTpe = agentLang(forAgent)
    replaceTypeArg(in, typeOf[Language], langTpe)
  }

  def TypesDefinitionsAgentSegment = MacroSegment{
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
            .mapValues(_.unzip._2.flatten.distinctBy(_.typeSymbol.name))
            .filter(_._2.nonEmpty)

          val typeDefs = abstractTypeMembers
            .mapValues(_.map(replaceLanguageTypeArg(_, tr)))
            .toList.map{ case (tName, ext :: mix) => q"type $tName = $ext with ..$mix" }

          name -> tr.prepend.body(typeDefs: _*)
        case system => system
      }

      trees.copy(agents = newAgs)
  }

  // depends on TypesDefinitions
  def CreateNegotiationAgentSegment = {

    def negotiationScopeUpdatedTree = q"{}"

    def createNegotiationTreeOpt(ag: ActorTrees) = {
      val implTpesOpt = ag.body
        .collect{
          case TypeDef(_, TypeName("Negotiation"), Nil, tree) =>
            tree match {
              case CompoundTypeTree(Template(tpes, _, _)) => tpes
            }
        }
        .ensuring(_.size <= 1).headOption

      val negotiationCreation = ag.parents.exists(_ <:< typeOf[impl.agent.NegotiationCreation])

      c.info(NoPosition, "!!!!!!!!!!!negotiationCreation = " + negotiationCreation, true)

      implTpesOpt map {
        impl =>
          q"""
            protected def createNegotiation(nId: NegotiationId): Negotiation = {
              val neg = new ${impl.head} with ..${impl.tail} {
                val id = nId
                val issues: Set[Var] = negotiationsInit.find(_.id == id).get.issues
                def scopeUpdated(): Unit = $negotiationScopeUpdatedTree
              }
              ${if(negotiationCreation) q"negotiationCreated(neg)" else q"" }
              log.debug("neg created(macro): " + neg)
              neg
            }
          """
      }

    }

    MacroSegment{
      case trees@Trees(_, ags) =>
        val newAgs = ags map {
          case (name, agTr) =>
            val opt = createNegotiationTreeOpt(agTr)
            name -> agTr.append.body(opt.toSeq: _*)
        }
        trees.copy(agents = newAgs)
    }
  }

  def transformIfHasMember(ags: Map[String, ActorTrees])(cond: Symbol => Boolean, f: ((String, ActorTrees)) => ActorTrees) =
    ags.map{
      case (name, ag) =>
        name -> ( if(ag.parents.exists(_.members.exists(cond))) f(name -> ag) else ag )
    }

  def isAbstract(name: Name): Symbol => Boolean = sym => sym.isAbstract && sym.name == name

  // requires EmbedIssuesAndDomainIteratorsCreators
  def DomainIteratorsAgentSegment = {
    def domainIteratorsArg = "domain-iterators"
    def domainIteratorsType = typeOf[Map[Var, DomainIteratorBuilder[Any, Any]]]
    def getDomainIterators = q"""
      issues.values.map(v => v -> domainIteratorsCreators(v.name)).toMap
    """
    def addDomainIteratorsArg(agName: String) = addAgentArgs(agName, domainIteratorsArg, domainIteratorsType, getDomainIterators)
    def domainIteratorsTree = q"""
      def domainIterators = args($domainIteratorsArg).asInstanceOf[Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]]]
    """

    MacroSegment{
      case trees@Trees(_, ags) =>
        val newAgs = transformIfHasMember(ags)(
          isAbstract(TermName("domainIterators")),
          {
            case (name, tr) =>
              addDomainIteratorsArg(name)
              tr.append.body(domainIteratorsTree)
          }
        )
        trees.copy(agents = newAgs)
    }

  }

  def ConstraintsByNegotiationAgentSegment = {

    def constraintsByNegotiationArg = "constraints-by-negotiation"
    def constraintsByNegotiationType = typeOf[Map[NegotiationId, NegotiationSpecification.AgentConstraintsDef]]
    def getConstraintsByNegotiation(agName: String) = q"""
      import feh.tec.agents.light.spec.NegotiationSpecification

      initialAgents.find(_._1.name == $agName).get._1.negotiations.map{
        case NegotiationSpecification.AgentNegDef(negName, _, extra) =>
          val constraints = extra.collect{
            case c: NegotiationSpecification.AgentConstraintsDef => c
          }
          NegotiationId(negName) -> constraints.ensuring(_.size == 1, constraints.size + " AgentConstraintsDef defined").head
      }.toMap
    """

    def addConstraintsByNegotiationArg(agName: String) =
      addAgentArgs(agName, constraintsByNegotiationArg, constraintsByNegotiationType, getConstraintsByNegotiation(agName))

    def seqToTuple = q"""
      (s: Seq[Any]) => ${
        Match(q"s", (for(i <- 2 to 22) yield
          cq"seq if seq.length == $i => ${TermName("Tuple"+i)}(..${for (j <- 0 until i) yield q"seq($j)"})".asInstanceOf[CaseDef]).toList)
    }"""

    def constraintsByNegotiationTree = q"""
      val constraintsByNegotiation = args($constraintsByNegotiationArg).asInstanceOf[$constraintsByNegotiationType]
      protected def seqToTuple(seq: Seq[Any]): Product = $seqToTuple(seq)
      """

    MacroSegment{
      case trees@Trees(_, ags) =>
        val newAgs = transformIfHasMember(ags)(
          isAbstract(TermName("constraintsByNegotiation")),
          {
            case (name, tr) =>
              addConstraintsByNegotiationArg(name)
              tr.append.body(constraintsByNegotiationTree)
          }
        )
        trees.copy(agents = newAgs)
    }

  }

  // depends on TypesDefinitionsAgentSegment
  def SpecAgentSegment(raw: NegotiationRaw) = {
    val specByAgentName = raw.agents.map(ag => ag.name -> ag.spec).toMap

    MacroSegment{
      case trees@Trees(_, ags) =>
        val newAgs = ags.map{
          case (name, tr) if specByAgentName contains name =>
            val agentType = tr.body.collect{
              case TypeDef(_, TypeName("Agent"), Nil, CompoundTypeTree(Template(parents, _, _))) =>
                internal.refinedType(parents.map(_.tpe), internal.newScopeWith())
            }.head
            val specTpe = tr.parents
              .flatMap(_.members.find(isAbstract(TermName("spec"))))
              .map(_.typeSignature.resultType).sortWith(_ <:< _)
              .last
              .pipe(replaceLanguageTypeArg(_, tr))
              .pipe(replaceTypeArg(_, typeOf[AbstractAgent], agentType))


            val sepcDef = q"val spec = ${specByAgentName(name)}.asInstanceOf[$specTpe]"
            name -> tr.append.body(sepcDef)
          case other => other
        }
        trees.copy(agents = newAgs)
    }
  }
}
