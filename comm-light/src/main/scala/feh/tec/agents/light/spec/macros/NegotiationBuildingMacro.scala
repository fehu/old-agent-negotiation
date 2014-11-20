package feh.tec.agents.light.spec.macros

import feh.tec.agents.light.impl.NegotiationEnvironmentController
import feh.tec.agents.light.spec
import feh.tec.agents.light.spec.{NegotiationSpecification, AgentSpecification}
import feh.tec.agents.light.spec.NegotiationSpecification.{InterlocutorsByRoles, NegotiationDef, Interlocutors}
import scala.concurrent.duration.FiniteDuration
import scala.reflect.internal.HasFlags
import scala.reflect.macros.whitebox

trait NegotiationBuildingMacro[C <: whitebox.Context] extends MacroContext[C]{

  case class NegotiationRaw(vars: List[Raw.VarDef],
                            negotiations: List[Raw.NegotiationDef],
                            agents: List[Raw.AgentDef],
                            spawns: List[Raw.SpawnDefs],
                            time: List[Raw.TimeDefs],
                            controller: Raw.ControllerDefs
                             )


  object Raw{
    type NegotiationDef = NegotiationSpecification.NegotiationDef

    case class VarDef(name: String, domain: DomainDef)
    case class DomainDef(domain: c.Tree, tpe: c.Type, domTpe: c.Type)

    case class SingleSpawnDef(name: String, count: c.Expr[Int])
    case class SpawnDefs(defs: Seq[SingleSpawnDef])

    case class AgentDef(name: String,
                        role: String,
                        negotiations: Seq[AgentNegDef],
                        spec: c.Expr[AgentSpecification])

    case class AgentNegDef(negotiation: String,
                           interlocutors: Interlocutors,
                           interlocutorsExpr: c.Expr[Interlocutors],
                           reportingToOpt: Option[c.Tree],
                           constraints: Seq[AgentConstraintsDef])
    case class AgentConstraintsDef(constraints: Seq[c.Tree])

    case class TimeDefs(mp: Map[String, c.Expr[FiniteDuration]])

    case class ControllerDefs(finished: Option[c.Expr[NegotiationEnvironmentController => (String, Seq[Map[String, Any]]) => Any]],
                              failed: Option[c.Expr[NegotiationEnvironmentController => (String, String) => Any]])
  }

  def build(dsl: c.Expr[spec.dsl.Negotiation]): NegotiationRaw

}

trait NegotiationBuildingMacroImpl[C <: whitebox.Context] extends NegotiationBuildingMacro[C]{
  def build(dsl: c.Expr[spec.dsl.Negotiation]): NegotiationRaw = {
    val b = new NegotiationSpecificationBuilder[c.type](c)
    b.build(dsl).asInstanceOf[NegotiationRaw]
  }
}

class NegotiationSpecificationBuilder[C <: whitebox.Context](val c: C) extends NegotiationBuildingMacro[C]{
  import c.universe._

  protected val h = new Helper[c.type](c)

  /** List(name -> body) */
  protected def extractRoot(root: c.Tree, knownDslNames: Set[String]) = {
    def extends_?(parents: List[c.Tree]) = parents.exists{
      p => knownDslNames exists {
        knownName => h.selects(p, knownName)
      }
    }
    val Some(defBody) = h.extractOne(root, {
      case cd@ClassDef(_, _, _, Template(parents, _, body)) if extends_?(parents) => body
      case unknown: ClassDef => c.abort(NoPosition, unknown.toString())
    })
    extractDefinitions(defBody) -> extractApplications(defBody)
  }

  protected def extractDefinitions(defBody: Seq[c.Tree]) = defBody.collect{
    case ValDef(_, name, _, body) => name -> body
    case DefDef(mods: HasFlags, name, Nil, Nil, _, body) if name != termNames.CONSTRUCTOR && !mods.hasAccessorFlag =>
      name -> body
  }

  protected def extractApplications(defBody: Seq[c.Tree]) = defBody.collect{
    case Apply(sel, args) if h.selectsSome(sel, "$anon") => sel -> args
  }


  /** Seq( (name, type, domain, domain type) ) */
  def extractVars(definitions: Seq[(c.TermName, c.Tree)]) = definitions collect {
    case (name, Apply(TypeApply(sel: Select, List(ttree@TypeTree())), arg :: Nil)) if h.selects(sel, "$anon.variable.with") =>
      val (dom, domTpe) = arg match {
        case Apply(
              Select(This(TypeName("$anon")), TermName("domain")),
              List(domainDef)
        ) =>
          val dTpe = extractDomainType(domainDef)
          domainDef -> dTpe
      }
      (name, ttree.tpe, dom, domTpe)
  }

  /** Seq(name -> List(var)) */
  def extractNegotiations(definitions: Seq[(c.TermName, c.Tree)]) = definitions collect{
    case (name, Apply(sel: Select, vars)) if h.selects(sel, "$anon.negotiation.over") => name -> vars
  }

  /**  Seq( (name, role, List[neg info], AgentSpec) )
    *  neg info:  (negotiation, interlocutors, Option(reportingTo)) -> List[raw constraint]
    */
  def extractAgents(definitions: Seq[(c.TermName, c.Tree)]) = definitions collect{
    case (name, Apply(
                  Select(
                    Apply(
                      TypeApply(
                        Select(
                          Apply(selWithRole: Select, Literal(Constant(role: String)) :: Nil),
                          TermName("definedBy" | "definedIn")
                          ),
                        List(specTTree@TypeTree())
                        ),
                      agentSpec :: Nil
                      ),
                    TermName("that")
                    ),
                  negDefs
                  )
                ) if h.selects(selWithRole, "$anon.agent.withRole") =>

      object ExtractNegotiation{
        def unapply(tree: c.Tree) = PartialFunction.condOpt(tree){
          case Apply(
                Apply(
                  TypeApply(
                    Select(
                      Apply(selNeg: Select,//Select(Select(This(TypeName("$anon")), TermName("negotiates")), TermName("the")),
                      List(negotiation)
                      ),
                    TermName("with")
                    ),
                  List(TypeTree())
                  ),
                List(interlocutors)
              ),
              _ //List(Select(This(TypeName("$anon")), TermName("TheRestOfSelectsInterlocutors")))
            ) if h.selects(selNeg, "$anon.negotiates.the") => negotiation -> interlocutors
        }
      }

      val negs = negDefs map {
        case Apply(negDef, confDefs) =>
          val negotiationsAndMore = negDef match {
            case Select(
                  Apply(
                    TypeApply(h.AnonSelect("chooseReporterToAgentNegPartialDef"), List(TypeTree())),
                    List(ExtractNegotiation(negotiation, interlocutors))
                    ),
                  TermName("and")
                  ) => (negotiation, interlocutors, None)
            case Select(
                  Apply(
                    TypeApply(
                      Select(
                        ExtractNegotiation(negotiation, interlocutors),
                        TermName("reportingTo")
                        ),
                      List(TypeTree())
                      ),
                    List(reportingTo)
                    ),
                  TermName("and")
                  ) => (negotiation, interlocutors, Some(reportingTo))
            case other => c.abort(NoPosition, "#extractAgents1 " + showRaw(other))
          }

          val constraints = confDefs flatMap {
            case Apply(sel: Select, constraintsTree) if h.selects(sel, "$anon.hasConstraints") => constraintsTree
          }
          negotiationsAndMore -> constraints
      }
      (name, role, negs, agentSpec/*, specTTree.tpe*/)
  }

  /** Seq(name -> count) */
  def extractSpawns(applications: Seq[(c.Tree, List[c.Tree])]) = applications.collect{
    case (Select(Select(This(TypeName("$anon")), TermName("spawn")), TermName("agents")), spawnDefs) =>
      spawnDefs map {
        case Apply(
              TypeApply(
                Select(
                  Apply(
                    TypeApply(
                      Select(Select(This(TypeName("scala")), TermName("Predef")), TermName("ArrowAssoc")),
                      List(TypeTree())
                      ),
                    List(Select(This(TypeName("$anon")), TermName(ag)))
                    ),
                  TermName("$minus$greater")
                  ),
                List(TypeTree())
                ),
              List(n)
              ) => ag -> c.Expr[Int](n)
      }
  }.flatten

  /** Timeouts: Seq(config_name -> duration) */
  def extractTimeouts(applications: Seq[(c.Tree, List[c.Tree])]) =
    applications.collect{
      case (Select(This(TypeName("$anon")), TermName("configure")), confDefs) =>
        confDefs map {
          case Apply(
                Select(Select(Select(This(TypeName("$anon")), TermName("timeout")), confName), TermName("$less$eq")),
                List(
                  duration@Select(
                    Apply(
                      Select(Select(Select(Select(Ident(TermName("scala")), TermName("concurrent")), TermName("duration")), termNames.PACKAGE), TermName("DurationInt")),
                      List(_)
                      ),
                    TermName(_)
                    )
                  )
                ) => (confName.decodedName.toString, duration)
        }
    }.flatten

  /** Seq( definition-name, Tree[function] ) */
  def extractControllerDefs(applications: Seq[(c.Tree, List[c.Tree])]) = applications collect {
    case (Select(Select(This(TypeName("$anon")), TermName("when")), TermName(defName)), tree) => defName -> tree
  }

  protected def extractDomainType(t: c.Tree) = t match {
    case Apply(
          Select(
            Apply(
              Select(
                _, //Select(This(TypeName("scala")), "Predef"),
                TermName("intWrapper")
                ),
              _ //List(_)
              ),
            TermName("to")
            ),
          _ //List(_)
          ) => c.typeOf[Range]
    case other =>
      c.abort(NoPosition, showRaw(other))
  }


  def build(dsl: c.Expr[spec.dsl.Negotiation]): NegotiationRaw = {
    import c.universe._

    val h = new Helper[c.type](c)
    val b = new NegotiationSpecificationBuilder[c.type](c)

    def KnownDSLNames = Set(
      "feh.tec.agents.light.spec.dsl.Negotiation"
    )

    val (definitions, applications) = b.extractRoot(dsl.tree, KnownDSLNames)

    val varsAndDomains = b.extractVars(definitions)
    val negotiationsAndIssues = b.extractNegotiations(definitions)
    val agentDefs = b.extractAgents(definitions)

    val vars = for ((name, tpe, domain, domainTpe) <- varsAndDomains) yield {
      val nme = name.decodedName.toString.trim
      Raw.VarDef(nme, Raw.DomainDef(domain, tpe, domainTpe))
    }

    val negotiations = for ((name, issues) <- negotiationsAndIssues) yield {
      val iss = issues.map{ case Select(This(TypeName("$anon")), varName) => varName.decodedName.toString }
      NegotiationDef(name.decodedName.toString, iss)
    }

    val agents = for ((name, role, negs, spec/*, specTpe*/) <- agentDefs) yield {
      val nr = negs.map{
        case ((negRaw, interlocutorsRaw, reportingToOpt), constraintsRaw) => // todo
          val neg = negRaw match { case Select(This(TypeName("$anon")), negName) => negName.decodedName.toString }
          val interlocutors = interlocutorsRaw match {
            case Select(Select(This(TypeName("$anon")), TermName("the")), TermName("others")) => Set(role)
            // todo: more cases
          }
          val constraints = Raw.AgentConstraintsDef(constraintsRaw)

          Raw.AgentNegDef(neg, InterlocutorsByRoles(interlocutors), c.Expr(q"InterlocutorsByRoles($interlocutors)"), reportingToOpt, List(constraints))
      }
      Raw.AgentDef(name.decodedName.toString, role, nr, c.Expr(spec)/*, specTpe*/)
    }

    def removeThisAndReplace(t: c.Tree, replacements: PartialFunction[c.Tree, c.Tree]) = h.transform(t,
      replacements orElse {
        case Select(This(_), name) => Ident(name)
      }
    )

    val controllerName = TermName("$arg_controller")
    val nameName = TermName("$arg_name")
    val valuesName = TermName("$arg_values")
    val reasonName = TermName("$arg_reason")

    val spawnsRaw = Raw.SpawnDefs(b.extractSpawns(applications) map (Raw.SingleSpawnDef.apply _).tupled)
//    val spawns = q"""SimpleSpawnDef(Map(..${b.extractSpawns(applications)}))"""
    val controllerEntries = extractControllerDefs(applications).groupBy(_._1).mapValues(_.unzip._2).map{
      case ("finished", funcs) =>
        val newFuncs = funcs.flatten.map{
          case q"($a) => ($b, $c) => $d" =>
            val CName = a.name
            val NName = b.name
            val VName = c.name
            removeThisAndReplace(d, {
              case Ident(CName) => Ident(controllerName)
              case Ident(NName) => Ident(nameName)
              case Ident(VName) => Ident(valuesName)
             })
        }
        "finished" -> q"""
          ($controllerName: feh.tec.agents.light.impl.NegotiationEnvironmentController) =>
            ($nameName: String, $valuesName: Seq[Map[String, Any]]) =>
              { ..$newFuncs }
        """
      case ("failed", funcs) =>
        val newFuncs = funcs.flatten.map{
          case q"($a) => ($b, $c) => $d" =>
            val CName = a.name
            val NName = b.name
            val RName = c.name
            removeThisAndReplace(d, {
              case Ident(CName) => Ident(controllerName)
              case Ident(NName) => Ident(nameName)
              case Ident(RName) => Ident(reasonName)
            })
        }
        "failed" -> q"""
          ($controllerName: feh.tec.agents.light.impl.NegotiationEnvironmentController) =>
            ($nameName: String, $reasonName: String) =>
              { ..$newFuncs }
        """
    }
    val controller = Raw.ControllerDefs(
      finished = controllerEntries.get("finished").map(c.Expr[(NegotiationEnvironmentController) => (String, scala.Seq[Map[String, Any]]) => Any]),
      failed = controllerEntries.get("failed").map(c.Expr[(NegotiationEnvironmentController) => (String, String) => Any])
    )

    val timeoutDefs = b.extractTimeouts(applications)
    val timeouts = Raw.TimeDefs(timeoutDefs.toMap.mapValues(c.Expr[FiniteDuration]))

    NegotiationRaw(vars.toList, negotiations.toList, agents.toList, spawnsRaw :: Nil, timeouts :: Nil, controller)
  }

}