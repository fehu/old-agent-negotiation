package feh.tec.agents.lite.spec.macros

import akka.actor.ActorRef
import feh.tec.agents.lite.AgentCreationInterface.NegotiationInit
import feh.tec.agents.lite._
import feh.tec.agents.lite.impl.agent.FailureChecks
import feh.tec.agents.lite.impl.{FailedConfigurationsChecks, agent}
import feh.tec.agents.lite.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.lite.spec.{AgentSpecification, NegotiationSpecification}
import feh.util._

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.macros.whitebox

trait AgentsBuildingMacro[C <: whitebox.Context] extends NegotiationBuildingMacro[C]{
  self: ControllerBuildingMacro[C] =>

  def AgentBuildSegments(negRaw: NegotiationRaw): List[MacroSegment]
  protected def agentArgsRequired(agent: String): Map[String, (c.Type, c.Tree)]
}


trait AgentsBuildingMacroImpl[C <: whitebox.Context] extends AgentsBuildingMacro[C]{
  self: ControllerBuildingMacro[C] =>

  import c.universe._

  def AgentBuildSegments(raw: NegotiationRaw) =
    EmptyAgentTrees(raw) ::
    PriorityAndProposalBasedAgentSegment(raw) ::
    IteratingAllVarsAgentSegment(raw) ::
    RequiresDistinctPriorityAgentSegment(raw) ::
    FailedConfigurationsChecksMacroSegment(raw) ::
    ReportingAgentSegment(raw) ::
    TypesDefinitionsAgentSegment ::
    CreateNegotiationAgentSegment ::
    DomainIteratorsAgentSegment ::
    ConstraintsByNegotiationAgentSegment ::
    SpecAgentSegment(raw) ::
    ResponseDelayAgentSegment :: Nil


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
    case NegotiationRaw(vars, negs, agents, _, _, _) =>
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
    case NegotiationRaw(_, _, agents, _, _, _) =>
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

  def FailedConfigurationsChecksMacroSegment(raw: NegotiationRaw) = {
    val failedConfigurationsChecks = raw.agents.filter(
      _.spec.actualType.member(TermName("agentTag"))
        .typeSignature.resultType.typeArgs.filter(_ <:< typeOf[AbstractAgent]).ensuring(_.size == 1)
        .head <:< typeOf[FailedConfigurationsChecks[_]]
    )
    val failedConfigurationsChecksTpe = typeOf[FailureChecks[Language.ProposalBased with Language.HasPriority]]

    //    val agTag = raw.agents.map(_.spec.actualType.member(TermName("agentTag"))).ensuring(_.size == 1).head
//    val agTpe = agTag.typeSignature.resultType.typeArgs.filter(_ <:< typeOf[AbstractAgent]).ensuring(_.size == 1).head

//    c.abort(NoPosition, showRaw(failedConfigurationsChecks))
    MacroSegment{
      case trees@Trees(_, ags) =>
//        c.abort(NoPosition, showRaw(ags))
        val newAgs = ags.map{
          transform(failedConfigurationsChecks){
            (tr, raw) =>
              val parent = replaceLanguageTypeArg(failedConfigurationsChecksTpe, tr)
              tr.append.parents(parent)
          }
        }
        trees.copy(agents = newAgs)
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

    def reportListenerRoleTree = q"feh.tec.agents.lite.impl.service.DefaultReportWriter.Role"
    def reportToTree = q"""
      systemAgents.find(_.id.role == $reportListenerRoleTree)
        .getOrElse(sys.error("no system agent for with " + $reportListenerRoleTree + " found"))
    """

    // todo
    def reportSysAgentBody: List[c.Tree] = reportingAgents.map(_.negotiations.map(_.reportingToOpt.get).distinct).flatMap(
      _.map{
        tree =>
          q"for(f <- $tree.forward) self ! feh.tec.agents.lite.AgentReport.Forward(f)"
      }
    ).toList

    def agentConstructorArgs = Map(
      "writeTo" -> typeOf[java.io.File],
      "controller" -> typeOf[ActorRef],
      "confirmAllWaitingDelay" -> typeOf[FiniteDuration]
    )

    def reportSysAgent = "report-listener" -> ActorTrees("$ReportListener",
      parents = typeOf[impl.service.DefaultReportWriter] :: typeOf[NegotiationFinishedListener] :: Nil, // todo: use ReportListenerRef
      body = reportSysAgentBody,
      constructorArgs = agentConstructorArgs
    )
    def addReportSysAgent() = {
      addAgentArgs("report-listener", "writeTo", typeOf[java.io.File], Ident(TermName("reportFile")) )
      addAgentArgs("report-listener", "role", typeOf[Role], reportListenerRoleTree)
      addAgentArgs("report-listener", "controller", typeOf[ActorRef], q"self")
      addAgentArgs("report-listener", "confirmAllWaitingDelay", typeOf[FiniteDuration], q"timeouts.`confirm finished`")
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
              .append.body(
                q"val reportingTo: Map[NegotiationId, AgentRef] = Map(..$reportingTo)",
                q"def extraMessage: Map[String, Any] = Map()",
                q"""def stateReport(negId: NegotiationId) =
                      feh.tec.agents.lite.StateReport(negId, get(negId).report(), "by demand")"""
              )
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

          c.info(NoPosition, typeDefs.map(showCode(_)).mkString("\n"), true)
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
      import feh.tec.agents.lite.spec.NegotiationSpecification

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

  def ResponseDelayAgentSegment = {

    def responseDelayArgName = "response-delay"
    def responseDelayArgType = typeOf[FiniteDuration]
    def responseDelayAgBody = q"""protected lazy val responseDelay = args($responseDelayArgName).asInstanceOf[$responseDelayArgType]"""
    def responseDelayControllerBody = q"timeouts.`response delay`"
    def addResponseDelayArg(agName: String) = addAgentArgs(agName, responseDelayArgName, responseDelayArgType, responseDelayControllerBody)

    MacroSegment{
      case trees@Trees(_, ags) =>
        val newAgs = ags.map{
          case (name, tr@ActorTrees(_, parents, _, _)) if parents.exists(_ <:< typeOf[ResponseDelay[Language]].erasure) =>
            addResponseDelayArg(name)
            name -> tr.append.body(responseDelayAgBody)
          case other => other
        }

        trees.copy(agents = newAgs)
    }

  }
}
