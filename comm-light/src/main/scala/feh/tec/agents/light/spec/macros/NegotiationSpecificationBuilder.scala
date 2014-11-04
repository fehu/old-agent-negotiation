package feh.tec.agents.light.spec.macros

import feh.tec.agents.light.spec.AgentSpecification
import feh.tec.agents.light.spec.NegotiationSpecification._
import feh.tec.agents.light.spec.macros.NegotiationSpecificationBuilder.Raw.TreesBuilder.ConstraintsBuilder

import scala.concurrent.duration.FiniteDuration
import scala.reflect.internal.HasFlags
import scala.reflect.macros.whitebox
import feh.tec.agents.light.spec
//import scala.language.experimental.macros

object NegotiationSpecificationBuilder{

  def build(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[spec.NegotiationSpecification] = {
    import c.universe._

    implicit val cb: ConstraintsBuilder = new SimpleConstraintsBuilder

    val ExtractRaw = Raw.BuildTrees[c.type](c)
    val ExtractRaw(vars, negotiations, agents, spawns, timeouts) = raw[c.type](c)(dsl)

    val specTree = q"""
      new feh.tec.agents.light.spec.NegotiationSpecification {
        import feh.tec.agents.light.spec.NegotiationSpecification._

        val variables: Seq[VarDef] = Seq(..$vars)
        val negotiations: Seq[NegotiationDef] = Seq(..$negotiations)
        val agents: Seq[AgentDef] = Seq(..$agents)

        val spawns: SpawnDef = $spawns
        val timeouts: TimeoutsDef = $timeouts
      }
    """

//    val s = "\tVARS:\n"           + (varsAndDomains map (showRaw(_)) mkString "\n") +
//            "\n\tNEGOTIATIONS\n"  + (negotiationsAndIssues map (showRaw(_)) mkString "\n") +
//            "\n\tAGENTS\n"        + (agentDefs map (showRaw(_)) mkString "\n")

//    val s = "\tVARS:\n"           + (vars mkString "\n") +
//            "\n\tNEGOTIATIONS\n"  + (negotiations mkString "\n") +
//            "\n\tAGENTS\n"        + (agents mkString "\n")

    c.Expr[spec.NegotiationSpecification](
//      q"""{ println($s);  $specTree }"""
      specTree
    )

  }


  case class Raw[C <: whitebox.Context](variables: Seq[Raw.VarDef[C]],
                                        negotiations: Seq[NegotiationDef],
                                        agents: Seq[Raw.AgentDef[C]],
                                        spawns: Raw.SpawnDefs[C],
                                        timeouts: Raw.TimeDefs[C],
       @deprecated("repeats variables") varsAndDomains:  Seq[(C#TermName, C#Type, C#Tree, C#Type)]
                                         )


  object Raw{
    final val NS = feh.tec.agents.light.spec.NegotiationSpecification

    object TreesBuilder{
      def vars[C <: whitebox.Context](c: C)(raw: Raw.VarDef[c.type]): c.Expr[NS.VarDef[_]] = {
        import c.universe._
        c.Expr(q"""
          import feh.tec.agents.light.spec.NegotiationSpecification._
          VarDef[Any](raw.name, GenericDomainDef(${raw.domain.domain.asInstanceOf[c.Tree]}, classOf[${raw.domain.tpe.asInstanceOf[c.Type]}]))
        """)
      }

      trait ConstraintsBuilder{
        def build[C <: whitebox.Context](c: C)(agDef: Raw.AgentConstraintsDef[c.type], raw: Raw[c.type]): c.Expr[NS.AgentConstraintsDef]
      }

      def agents[C <: whitebox.Context](c: C)
                                       (agDef: Raw.AgentDef[C], raw: Raw[C])
                                       (implicit cb: ConstraintsBuilder): c.Expr[NS.AgentDef] = {
        import c.universe._

        agDef match{
          case AgentDef(name, role, negDefs, specExpr/*, specTpe*/) =>
            val negs = negDefs map {
              case AgentNegDef(neg, _, interlExpr, reportingToOpt /*ignored*/, constraints) =>
                q"""
                    import feh.tec.agents.light.spec.NegotiationSpecification._
                    AgentNegDef(
                      $neg,
                      ${interlExpr.tree.asInstanceOf[c.Tree]},
                      Seq(..${
                        constraints
                          .asInstanceOf[Seq[Raw.AgentConstraintsDef[c.type]]]
                          .map(cb.build[c.type](c)(_, raw.asInstanceOf[Raw[c.type]]))
                      })
                    )
                    """
            }
            c.Expr(q"AgentDef($name, $role, Seq(..$negs), ${specExpr.tree.asInstanceOf[c.Tree]})")

        }
      }

      def spawns[C <: whitebox.Context](c: C)(raw: Raw.SpawnDefs[C]): c.Expr[NS.SpawnDef] = {
        import c.universe._

        raw match {
          case SpawnDefs(sd) =>
            val mapEntries = sd map{ case SingleSpawnDef(name, countExpr) => q"$name -> ${countExpr.tree.asInstanceOf[c.Tree]}" }
            c.Expr(q"""
              import feh.tec.agents.light.spec.NegotiationSpecification._
              SimpleSpawnDef(Map(Seq(..$mapEntries): _*))"""
            )
        }
      }

      def timeouts[C <: whitebox.Context](c: C)(raw: Raw.TimeDefs[C]): c.Expr[NS.TimeoutsDef] = {
        import c.universe._
        val mapEntries = raw.mp.map{ case (name, expr) => q"$name -> ${expr.tree.asInstanceOf[c.Tree]}"}
        c.Expr(q"""
          import feh.tec.agents.light.spec.NegotiationSpecification._
          TimeoutsDef(Map(Seq(..$mapEntries): _*))"""
        )
      }

    }

    case class TreesBuilder[C <: whitebox.Context](raw: Raw[_ <: whitebox.Context], c: C)
                                                  (implicit cb: ConstraintsBuilder){
      import c.universe._

      lazy val variables: Seq[c.Expr[NS.VarDef[_]]] = raw.variables.map{
        rvd => TreesBuilder.vars[c.type](c)(rvd.asInstanceOf[Raw.VarDef[c.type]])
      }

      lazy val negotiations = raw.negotiations.map{ case NegotiationDef(name, issues) => reify(NegotiationDef(name, issues))}

      lazy val agents: Seq[c.Expr[NS.AgentDef]] = raw.agents.map{
        rad => TreesBuilder.agents[c.type](c)(rad.asInstanceOf[Raw.AgentDef[c.type]], raw.asInstanceOf[Raw[c.type]])
      }

      lazy val spawns: c.Expr[NS.SpawnDef] = TreesBuilder.spawns[c.type](c)(raw.spawns.asInstanceOf[Raw.SpawnDefs[c.type]])
      lazy val timeouts: C#Expr[NS.TimeoutsDef] = TreesBuilder.timeouts[c.type](c)(raw.timeouts.asInstanceOf[Raw.TimeDefs[c.type]])
    }

    def BuildTrees[Context <: whitebox.Context](c: Context)(implicit cb: ConstraintsBuilder) = new {
      def unapply[C <: whitebox.Context](raw: Raw[C]) ={
        val extr = TreesBuilder[c.type](raw, c)
        Some((
          extr.variables, extr.negotiations, extr.agents, extr.spawns, extr.timeouts
          ))
      }
    }

    case class VarDef[C <: whitebox.Context](name: String, domain: DomainDef[C])
    case class DomainDef[C <: whitebox.Context](domain: C#Tree, tpe: C#Type, domTpe: C#Type)

    case class SingleSpawnDef[C <: whitebox.Context](name: String, count: C#Expr[Int])
    case class SpawnDefs[C <: whitebox.Context](defs: Seq[SingleSpawnDef[C]])

    case class AgentNegDef[C <: whitebox.Context](negotiation: String,
                                                  interlocutors: Interlocutors,
                                                  interlocutorsExpr: C#Expr[Interlocutors],
                                                  reportingToOpt: Option[C#Tree],
                                                  constraints: Seq[AgentConstraintsDef[C]])
    case class AgentDef[C <: whitebox.Context](name: String,
                                               role: String,
                                               negotiations: Seq[AgentNegDef[C]],
                                               spec: C#Expr[AgentSpecification])
//                                               specTpe: C#Type
    case class AgentConstraintsDef[C <: whitebox.Context](constraints: Seq[C#Tree])

    case class TimeDefs[C <: whitebox.Context](mp: Map[String, C#Expr[FiniteDuration]])

  }

  def raw[C <: whitebox.Context](c: C)(dsl: c.Expr[spec.dsl.Negotiation]): Raw[C] = {
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
      Raw.VarDef[C](nme, Raw.DomainDef[C](domain, tpe, domainTpe))
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
          val constraints = Raw.AgentConstraintsDef[C](constraintsRaw)

          Raw.AgentNegDef[C](neg, InterlocutorsByRoles(interlocutors), c.Expr(q"InterlocutorsByRoles($interlocutors)"), reportingToOpt, List(constraints))
      }
      Raw.AgentDef[C](name.decodedName.toString, role, nr, c.Expr(spec)/*, specTpe*/)
    }

    val spawnsRaw = Raw.SpawnDefs[C](b.extractSpawns(applications) map (Raw.SingleSpawnDef.apply[C] _).tupled)
    val spawns = q"""SimpleSpawnDef(Map(..${b.extractSpawns(applications)}))"""

    val timeoutDefs = b.extractTimeouts(applications)
    val timeouts = Raw.TimeDefs[C](timeoutDefs.toMap.mapValues(c.Expr(_))) //q"""TimeoutsDef(Map(..$timeoutDefs))"""

    Raw[C](vars, negotiations, agents, spawnsRaw, timeouts, varsAndDomains)
  }


  class SimpleConstraintsBuilder extends ConstraintsBuilder{

    def build[C <: whitebox.Context](c: C)(agDef: Raw.AgentConstraintsDef[c.type], raw: Raw[c.type]): c.Expr[AgentConstraintsDef] = {

      val xc = new ExtendedConstraint[c.type](c)
        import c.universe._

        val constraints = agDef.constraints.asInstanceOf[Seq[c.Tree]] map xc.extractConstraintsDef map {
          case (cName, xc.Replacement(descr, f)) =>
            val func = f(raw.varsAndDomains.map(tr => tr._1.decodedName.toString.trim -> tr._2).toMap)
            val descriptions = descr map {
              case xc.Description(tpe, varName, arg) =>
              q"""ConstraintParamDescription($tpe, $varName, ${arg.decodedName.toString})"""
            }
            q"""AgentConstraintDef($cName, Seq(..$descriptions), $func.tupled.asInstanceOf[Product => Boolean])"""
        }
        c.Expr(q"AgentConstraintsDef(Seq(..$constraints))")
      }
  }

  class VarsSeparatingConstraintsBuilder extends ConstraintsBuilder{

    def build[C <: whitebox.Context](c: C)(agConstrDef: Raw.AgentConstraintsDef[c.type], raw: Raw[c.type]): c.Expr[AgentConstraintsDef] = {
      import c.universe._

      val h = new Helper[c.type](c)
      import h._

      val xc = new ExtendedConstraint[c.type](c)

      implicit object TreeConstraintPartIsLiftable extends Liftable[ConstraintPart[c.Tree]]{

        def apply(value: ConstraintPart[c.Tree]): c.Tree = value match {
          case ConstraintPartComb(op, left, right) =>
            val opStr = op.toString match{
              case "$amp$amp" => "and"
              case "$bar$bar" => "or"
            }
            q"ConstraintPartComb($opStr, ${apply(left)}, ${apply(right)})"
          case ConstraintPartLeaf(leaf) => q"ConstraintPartLeaf($leaf)"
        }
      }

//      val constraints = agConstrDef.constraints.asInstanceOf[Seq[c.Tree]] map xc.extractConstraintsDef map {
//        case (cName, xc.Replacement(descr, f)) =>
//      }

      def separateAndOr(in: c.Tree): ConstraintPart[c.Tree] = in match {
        case Apply(Select(left, op@(TermName("$bar$bar") | TermName("$amp$amp"))), List(right)) =>
          ConstraintPartComb[c.Tree, c.TermName](op.asInstanceOf[c.TermName], separateAndOr(left), separateAndOr(right))
        case other if other.tpe == typeOf[Boolean] => ConstraintPartLeaf[c.Tree](other)
      }
      
      val separatedByName = agConstrDef.constraints.asInstanceOf[Seq[c.Tree]].map{
        case Apply(
              Select(
                Apply(
                  TypeApply(AnonSelect("VarDefConstraintBuilder"), List(TypeTree())),
                  List(Literal(Constant(constraintName: String)))
                  ),
                TermName("$bar")
                ),
              List(constraintTree)
            ) =>
          constraintName -> separateAndOr(constraintTree)
      }.toMap//groupBy(_._1).mapValues(_.map(_._2))

      val replacementsByName = separatedByName.mapValues(
        _.transform(xc.replace, (n: c.TermName) => n.toString) //[(String, ExtendedConstraint#Replacement), TermName, String]
      )

      val agentVarConstraintDefByName = replacementsByName.mapValues{
        _.transform(
            {
              case xc.Replacement(descr, f) =>
                val func = f(raw.varsAndDomains.map(tr => tr._1.decodedName.toString.trim -> tr._2).toMap)
                val descriptions = descr map {
                  case xc.Description(tpe, varName, arg) =>
                    q"""ConstraintParamDescription($tpe, $varName, ${arg.decodedName.toString})"""
                }
              q"""AgentVarConstraintDef(Seq(..$descriptions), $func.tupled.asInstanceOf[Product => Boolean])"""
            },
          identity[String]
          )
      }

      val constraints = agentVarConstraintDefByName.map{
        case (name, agentVarConstraintDef) => q"AgentConstraintDef($name, $agentVarConstraintDef)"
      }

      c.Expr(q"AgentConstraintsDef(Seq(..$constraints))")
    }
  }

}

class NegotiationSpecificationBuilder[C <: whitebox.Context](val c: C){
  import c.universe._

  val h = new Helper[c.type](c)

  /** List(name -> body) */
  def extractRoot(root: c.Tree, knownDslNames: Set[String]) = {
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

  def extractDefinitions(defBody: Seq[c.Tree]) = defBody.collect{
    case ValDef(_, name, _, body) => name -> body
    case DefDef(mods: HasFlags, name, Nil, Nil, _, body) if name != termNames.CONSTRUCTOR && !mods.hasAccessorFlag =>
      name -> body
  }

  def extractApplications(defBody: Seq[c.Tree]) = defBody.collect{
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

}