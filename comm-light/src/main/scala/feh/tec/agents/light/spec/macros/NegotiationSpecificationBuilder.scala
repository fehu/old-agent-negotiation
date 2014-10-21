package feh.tec.agents.light.spec.macros

import feh.tec.agents.light.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.light.spec.AgentProps.AgentPropsBundle
import feh.tec.agents.light.spec.NegotiationSpecification
import feh.tec.agents.light.spec.{ NegotiationSpecification => NS }

import scala.reflect.internal.HasFlags
import scala.reflect.macros.whitebox
import feh.tec.agents.light.spec
//import scala.language.experimental.macros

object NegotiationSpecificationBuilder{

  def build(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[spec.NegotiationSpecification] = {
    import c.universe._

    val ExtractRaw = Raw.Extract[c.type](c)
    val ExtractRaw(vars, negotiations, agents, spawns, timings, timeouts) = raw[c.type](c)(dsl)

    val specTree = q"""
      new feh.tec.agents.light.spec.NegotiationSpecification {
        import feh.tec.agents.light.spec.NegotiationSpecification._

        def variables: Seq[VarDef] = Seq(..$vars)
        def negotiations: Seq[NegotiationDef] = Seq(..$negotiations)
        def agents: Seq[AgentDef] = Seq(..$agents)

        def spawns: SpawnDef = $spawns
        def timings: TimingsDef = $timings
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

  case class Raw[C <: whitebox.Context](variables: Seq[C#Expr[NS.VarDef[_]]],
                                        negotiations: Seq[C#Expr[NS.NegotiationDef]],
                                        agents: Seq[C#Expr[NS.AgentDef]],
                                        spawns: C#Expr[NS.SpawnDef],
                                        timings: C#Expr[NS.TimingsDef],
                                        timeouts: C#Expr[NS.TimeoutsDef]
                                   )

  object Raw{
    def Extract[Context <: whitebox.Context](c: Context) = new {
      def unapply[C <: whitebox.Context](raw: Raw[C]) =
        Some(
          raw.variables.map(_.asInstanceOf[c.Expr[NS.VarDef[_]]]),
          raw.negotiations.map(_.asInstanceOf[c.Expr[NS.NegotiationDef]]),
          raw.agents.map(_.asInstanceOf[c.Expr[NS.AgentDef]]),
          raw.spawns.asInstanceOf[c.Expr[NS.SpawnDef]],
          raw.timings.asInstanceOf[c.Expr[NS.TimingsDef]],
          raw.timeouts.asInstanceOf[c.Expr[NS.TimeoutsDef]]
          )
    }
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
      q"""val clazz = classOf[$tpe].asInstanceOf[Class[$tpe]]
          VarDef[$tpe](${name.decodedName.toString.trim}, GenericDomainDef[$tpe, $domainTpe]($domain, clazz))
          """
    }

    val negotiations = for ((name, issues) <- negotiationsAndIssues) yield {
      val iss = issues.map{ case Select(This(TypeName("$anon")), varName) => varName.decodedName.toString }
      q"""NegotiationDef(${name.decodedName.toString}, $iss)"""
    }

    val agents = for ((name, role, negs, spec) <- agentDefs) yield {
      val n = negs map {
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

          q"""AgentNegDef($neg, InterlocutorsByRoles($interlocutors), List(
                  AgentConstraintsDef(Seq(..$constraints))
                ))"""
      }
      q"""AgentDef(${name.decodedName.toString}, $role, Seq(..$n), $spec)"""
    }

    val spawns = q"""SimpleSpawnDef(Map(..${b.extractSpawns(applications)}))"""

    val (timeoutDefs, timingDefs) = b.extractTimeoutsAndTimings(applications)
    val timeouts = q"""TimeoutsDef(Map(..$timeoutDefs))"""
    val timings = q"""TimingsDef(Map(..$timingDefs))"""

    Raw[C](vars.map(c.Expr(_)), negotiations.map(c.Expr(_)), agents.map(c.Expr(_)), c.Expr(spawns), c.Expr(timings), c.Expr(timeouts))
  }


  def agentPropsBundle[C <: whitebox.Context](c: C)(specTree: c.Tree): c.Expr[AgentPropsBundle[_]] = {
    import c.universe._

    object spec{
      var PriorityAndProposalBased = false
      var iterating: Iterating.Value = Iterating.None

      object Iterating extends Enumeration{ val All, CurrentIssues, None = Value }
    }
    def typeCheck[T : c.TypeTag] = c.typecheck(specTree, c.TERMmode, c.typeOf[T], silent = true).nonEmpty
    def typeCheckAndSet[T : c.TypeTag](set: => Unit) = if(typeCheck[T]) set

    typeCheckAndSet[PriorityAndProposalBasedAgentSpec[_, _]]{ spec.PriorityAndProposalBased = true }
    typeCheckAndSet[IteratingSpec.AllVars[_, _]]{ spec.iterating = spec.Iterating.All }


    c.abort(NoPosition, s"PriorityAndProposalBased=${spec.PriorityAndProposalBased}, iterating=${spec.iterating}")
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
             ) => ag -> n
      }
  }.flatten

  /** (Timeouts, Timings): Seq(config_name -> duration) -> Seq(config_name -> duration) */
  def extractTimeoutsAndTimings(applications: Seq[(c.Tree, List[c.Tree])]) = {
    val mp = applications.collect{
      case (Select(This(TypeName("$anon")), TermName("configure")), confDefs) =>
        confDefs map {
          case Apply(
            Select(Select(Select(This(TypeName("$anon")), TermName(tpe)), confName), TermName("$less$eq")),
              List(
       duration@Select(
                  Apply(
                    Select(Select(Select(Select(Ident(TermName("scala")), TermName("concurrent")), TermName("duration")), termNames.PACKAGE), TermName("DurationInt")),
                    List(_)
                    ),
                  TermName(_)
                  )
                )
              ) => tpe -> (confName.decodedName.toString, duration)
        }
    }.flatten.groupBy(_._1).mapValues(_.map(_._2))

    mp.getOrElse("timeout", Nil) -> mp.getOrElse("timing", Nil)
  }

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