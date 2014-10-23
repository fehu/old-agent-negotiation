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

    implicit val cb: ConstraintsBuilder = ???

    val ExtractRaw = Raw.BuildTrees[c.type](c)
    val ExtractRaw(vars, negotiations, agents, spawns, timeouts) = raw[c.type](c)(dsl)

    val specTree = q"""
      new feh.tec.agents.light.spec.NegotiationSpecification {
        import feh.tec.agents.light.spec.NegotiationSpecification._

        def variables: Seq[VarDef] = Seq(..$vars)
        def negotiations: Seq[NegotiationDef] = Seq(..$negotiations)
        def agents: Seq[AgentDef] = Seq(..$agents)

        def spawns: SpawnDef = $spawns
        def timeouts: TimeoutsDef = $timeouts
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

/*
  case class Raw[C <: whitebox.Context](variablesWithRaw: Seq[(Raw.VarDef[C], C#Expr[Raw.NS.VarDef[_]])],
                                        negotiations: Seq[C#Expr[Raw.NS.NegotiationDef]],
                                        agentsWithRaw: Seq[(Raw.AgentDef[C], C#Expr[Raw.NS.AgentDef])],
                                        spawnsWithRaw: (Raw.SpawnDefs[C], C#Expr[Raw.NS.SpawnDef]),
                                        timings: C#Expr[Raw.NS.TimingsDef],
                                        timeouts: C#Expr[Raw.NS.TimeoutsDef]
                                   ){
    def rawVariables = variablesWithRaw.map(_._1)
//    def rawNegotiations = negotiationsWithRaw.map(_._1)
    def rawAgents = agentsWithRaw.map(_._1)
    def rawSpawns = spawnsWithRaw._1
//    def rawTimings = timingsWithRaw._1
//    def rawTimeouts = timeoutsWithRaw._1

    def variables: Seq[C#Expr[VarDef[_]]] = variablesWithRaw.map(_._2)
//    def negotiations: Seq[C#Expr[NegotiationDef]] = negotiationsWithRaw.map(_._2)
    def agents: Seq[C#Expr[AgentDef]] = agentsWithRaw.map(_._2)
    def spawns: C#Expr[SpawnDef] = spawnsWithRaw._2
//    def timings: C#Expr[TimingsDef] = timingsWithRaw._2
//    def timeouts: C#Expr[TimeoutsDef] = timeoutsWithRaw._2
  }
*/

  case class Raw[C <: whitebox.Context](variables: Seq[Raw.VarDef[C]],
                                        negotiations: Seq[NegotiationDef],
                                        agents: Seq[Raw.AgentDef[C]],
                                        spawns: Raw.SpawnDefs[C],
//                                        timings: Raw.TimeDefs[C],
                                        timeouts: Raw.TimeDefs[C]
                                         )

  
  object Raw{
    final val NS = feh.tec.agents.light.spec.NegotiationSpecification

    object TreesBuilder{
      def vars[C <: whitebox.Context](c: C)(raw: Raw.VarDef[c.type]): c.Expr[NS.VarDef[_]] = {
        import c.universe._
        c.Expr(q"""
          NS.VarDef[Any](raw.name, NS.GenericDomainDef(${raw.domain.domain.asInstanceOf[c.Tree]}, classOf[${raw.domain.tpe.asInstanceOf[c.Type]}]))
        """)
      }

      trait ConstraintsBuilder{
        def build[C <: whitebox.Context](c: C)(raw: Raw.AgentConstraintsDef[c.type]): c.Expr[NS.AgentConstraintDef]
      }
      
      def agents[C <: whitebox.Context](c: C)(raw: Raw.AgentDef[C])(implicit cb: ConstraintsBuilder): c.Expr[NS.AgentDef] = {
        import c.universe._

        raw match{
          case AgentDef(name, role, negDefs, specExpr) =>
            val negs = negDefs map {
              case AgentNegDef(neg, _, interlExpr, constraints) =>
                q"""NS.AgentNegDef(
                      $neg,
                      ${interlExpr.tree.asInstanceOf[c.Tree]},
                      Seq(..${constraints.asInstanceOf[Seq[Raw.AgentConstraintsDef[c.type]]].map(cb.build[c.type](c)(_))}))
                    """
            }
            c.Expr(q"NS.AgentDef($name, $role, Seq(..$negs), ${specExpr.tree.asInstanceOf[c.Tree]})")

        }
      }

      def spawns[C <: whitebox.Context](c: C)(raw: Raw.SpawnDefs[C]): c.Expr[NS.SpawnDef] = {
        import c.universe._

        raw match {
          case SpawnDefs(sd) =>
            val mapEntries = sd map{ case SingleSpawnDef(name, countExpr) => q"$name -> ${countExpr.tree.asInstanceOf[c.Tree]}" }
            c.Expr(q"NS.SpawnDef(Map(Seq(..$mapEntries): _*))")
        }
      }

      def timeouts[C <: whitebox.Context](c: C)(raw: Raw.TimeDefs[C]): c.Expr[NS.TimeoutsDef] = {
        import c.universe._
        val mapEntries = raw.mp.map{ case (name, expr) => q"$name -> ${expr.tree.asInstanceOf[c.Tree]}"}
        c.Expr(q"NS.TimeoutsDef(Map(Seq(..$mapEntries): _*))")
      }

    }

    case class TreesBuilder[C <: whitebox.Context](raw: Raw[_ <: whitebox.Context], c: C)(implicit cb: ConstraintsBuilder){
      import c.universe._

      lazy val variables: Seq[c.Expr[NS.VarDef[_]]] = raw.variables.map{
        rvd => TreesBuilder.vars[c.type](c)(rvd.asInstanceOf[Raw.VarDef[c.type]])
      }

      lazy val negotiations = raw.negotiations.map{ case NegotiationDef(name, issues) => reify(NegotiationDef(name, issues))}
      
      lazy val agents: Seq[c.Expr[NS.AgentDef]] = raw.agents.map{
        rad => TreesBuilder.agents[c.type](c)(rad.asInstanceOf[Raw.AgentDef[c.type]])
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
                                                  constraints: Seq[AgentConstraintsDef[C]])
    case class AgentDef[C <: whitebox.Context](name: String, role: String, negotiations: Seq[AgentNegDef[C]], spec: C#Expr[AgentSpecification])
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

    val agents = for ((name, role, negs, spec) <- agentDefs) yield {
      val nr = negs.map{
        case ((negRaw, interlocutorsRaw), constraintsRaw) =>
          val neg = negRaw match { case Select(This(TypeName("$anon")), negName) => negName.decodedName.toString }
          val interlocutors = interlocutorsRaw match {
            case Select(Select(This(TypeName("$anon")), TermName("the")), TermName("others")) => Set(role)
            // todo: more cases
          }
          val constraints = constraintsRaw map{
            case (cName, b.xc.Replacement(descr, f)) =>
              val func = f(varsAndDomains.map(tr => tr._1.decodedName.toString.trim -> tr._2).toMap)
              val descriptions = descr map {
                case b.xc.Description(tpe, varName, arg) =>
                  q"""ConstraintParamDescription($tpe, $varName, ${arg.decodedName.toString})"""
              }
              q"""AgentConstraintDef($cName, Seq(..$descriptions), $func.tupled.asInstanceOf[Product => Boolean])"""
          }

          Raw.AgentNegDef[C](neg, InterlocutorsByRoles(interlocutors), c.Expr(q"InterlocutorsByRoles($interlocutors)"), List(Raw.AgentConstraintsDef[C](constraints)))
      }
      Raw.AgentDef[C](name.decodedName.toString, role, nr, c.Expr(spec))
    }

    val spawnsRaw = Raw.SpawnDefs[C](b.extractSpawns(applications) map (Raw.SingleSpawnDef.apply[C] _).tupled)
    val spawns = q"""SimpleSpawnDef(Map(..${b.extractSpawns(applications)}))"""

    val timeoutDefs = b.extractTimeouts(applications)
    val timeouts = Raw.TimeDefs[C](timeoutDefs.toMap.mapValues(c.Expr(_))) //q"""TimeoutsDef(Map(..$timeoutDefs))"""

    Raw[C](vars, negotiations, agents, spawnsRaw, timeouts)
  }

}

class NegotiationSpecificationBuilder[C <: whitebox.Context](val c: C){
  import c.universe._

  val h = new Helper[c.type](c)
  val xc = new ExtendedConstraint[c.type](c)

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

  /**  Seq( (name, role, List[constraint name -> ExtendedConstraint#Replaced], AgentSpec) ) */
  def extractAgents(definitions: Seq[(c.TermName, c.Tree)]) = definitions collect{
    case (name, Apply(
                  Select(
                    Apply(
                      Select(
                        Apply(selWithRole: Select, Literal(Constant(role: String)) :: Nil),
                        TermName("definedBy" | "definedIn")
                      ),
                      agentSpec :: Nil
                    ),
                    TermName("that")
                  ),
                  negDefs
                )
          ) if h.selects(selWithRole, "$anon.agent.withRole") =>

      val negs = negDefs map {
        case Apply(negDef, confDefs) =>
          val negotiationsAndInterlocutors = negDef match {
            case Select(
                  Apply(
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
                  ),
                TermName("and")
                ) if h.selects(selNeg, "$anon.negotiates.the") => negotiation -> interlocutors
          }

          val config = confDefs flatMap {
            case Apply(sel: Select, constraints) if h.selects(sel, "$anon.hasConstraints") =>
              constraints map extractConstraintsDef
          }
          negotiationsAndInterlocutors -> config
      }
      (name, role, negs, agentSpec)
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

  protected def extractConstraintsDef(t: c.Tree) = t match {
    case Apply(
          Select(
            Apply(
              TypeApply(
                Select(This(TypeName("$anon")), TermName("VarDefConstraintBuilder")),
                List(TypeTree())
              ),
            List(Literal(Constant(name: String)))
            ),
          TermName("$bar")
          ),
          List( constraint )
        ) => name -> xc.replace(constraint)
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

}