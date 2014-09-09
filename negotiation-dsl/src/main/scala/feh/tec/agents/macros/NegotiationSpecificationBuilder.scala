package feh.tec.agents.macros

import feh.tec.agents.impl.NegotiationSpecification.{GenericDomainDef, VarDef}
import feh.tec.agents.impl.{NegotiationSpecification, NegotiationSpecificationDSL}
import scala.reflect.internal.HasFlags
import scala.reflect.macros.whitebox

object NegotiationSpecificationBuilder{

  def build(c: whitebox.Context)(dsl: c.Expr[NegotiationSpecificationDSL]): c.Expr[NegotiationSpecification] = {
    import c.universe._

    val h = new Helper[c.type](c)
    val b = new NegotiationSpecificationBuilder[c.type](c)

    def DSLClassName = "feh.tec.agents.impl.NegotiationSpecificationDSL"

    val definitions = b.extractDefinitions(dsl.tree, DSLClassName)

    val varsAndDomains = b.extractVars(definitions)
    val negotiationsAndIssues = b.extractNegotiations(definitions)
    val agentDefs = b.extractAgents(definitions)

//    def applications =

    val vars = for ((name, tpe, domain, domainTpe) <- varsAndDomains) yield {
      q"""val clazz = classOf[$tpe].asInstanceOf[Class[$tpe]]
          VarDef[$tpe](${name.decodedName.toString.trim}, GenericDomainDef[$tpe, $domainTpe]($domain, clazz))
          """
    }


    val negotiations = for ((name, issues) <- negotiationsAndIssues) yield {
      val iss = issues.map{ case Select(This(TypeName("$anon")), varName) => varName.decodedName.toString }
      q"""NegotiationDef(${name.decodedName.toString}, $iss)"""
    }

    val agents = for ((name, role, negs) <- agentDefs) yield {
      val n = negs map {
          case ((negRaw, interlocutorsRaw), constraintsRaw) =>
            val neg = negRaw match { case Select(This(TypeName("$anon")), negName) => negName.decodedName.toString }
            val interlocutors = interlocutorsRaw match {
              case Select(Select(This(TypeName("$anon")), TermName("the")), TermName("others")) => Set(role)
              // todo: more cases
            }
            val constraints = constraintsRaw map{
              case (cName, b.xc.Replaced(descr, func)) =>
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
      q"""AgentDef(${name.decodedName.toString}, $role, Seq(..$n))"""
    }

    val spec = q"""
      new feh.tec.agents.impl.NegotiationSpecification {
        import feh.tec.agents.impl.NegotiationSpecification._

        def variables: Seq[VarDef] = Seq(..$vars)
        def negotiations: Seq[NegotiationDef] = Seq(..$negotiations)
        def agents: Seq[AgentDef] = Seq(..$agents)

        def spawns: SpawnDef = null
        def timings: TimingsDef = null
        def timeouts: TimeoutsDef = null
      }
    """

//    val s = "\tVARS:\n"           + (varsAndDomains map (showRaw(_)) mkString "\n") +
//            "\n\tNEGOTIATIONS\n"  + (negotiationsAndIssues map (showRaw(_)) mkString "\n") +
//            "\n\tAGENTS\n"        + (agentDefs map (showRaw(_)) mkString "\n")

//    val s = "\tVARS:\n"           + (vars mkString "\n") +
//            "\n\tNEGOTIATIONS\n"  + (negotiations mkString "\n") +
//            "\n\tAGENTS\n"        + (agents mkString "\n")


    c.Expr[NegotiationSpecification](
//      q"""{ println($s);  $spec }"""
      spec
    )

  }
}

class NegotiationSpecificationBuilder[C <: whitebox.Context](val c: C){
  import c.universe._

  val h = new Helper[c.type](c)
  val xc = new ExtendedConstraint[c.type](c)

  /** List(name -> body) */
  def extractDefinitions(root: c.Tree, DSLClassName: String) = {
    val Some(defBody) = h.extractOne(root, {
      case cd@ClassDef(_, _, _, Template(parents, _, body)) if parents.exists(h.selects(_, DSLClassName)) => body
    })

    defBody.collect{
      case ValDef(_, name, _, body) => name -> body
      case DefDef(mods: HasFlags, name, Nil, Nil, _, body) if name != termNames.CONSTRUCTOR && !mods.hasAccessorFlag =>
        name -> body
    }
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

  /**  Seq( (name, role, List[constraint name -> ExtendedConstraint#Replaced]) ) */
  def extractAgents(definitions: Seq[(c.TermName, c.Tree)]) = definitions collect{
    case (name, Apply(
                  Select(Apply(sel: Select, Literal(Constant(r: String)) :: Nil), TermName("that")),
                  negDefs
                )
          ) if h.selects(sel, "$anon.agent.withRole") =>

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
      (name, r, negs)
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