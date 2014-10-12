package feh.tec.agents.light.spec.macros

import feh.tec.agents.light.spec
import scala.reflect.macros.whitebox

object AgentSpecificationBuilder {
  def build(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Agent]): c.Expr[spec.AgentSpecification] = {
    import c.universe._

    val h = new Helper[c.type](c)
    import h.Wrapper

    lazy val SpecName = "feh.tec.agents.light.spec.dsl.Agent"

    val builders = Map(
      "feh.tec.agents.light.spec.dsl.PriorityAndProposalBased" -> new PriorityAndProposalBasedBuilder[c.type](c)
    )

    val Some((parents, body)) = dsl.tree extractOne {
      case ClassDef(_, TypeName("$anon"), _, Template(parents, `noSelfType`, body)) if parents.exists(_ selects SpecName) =>
        parents.collect{ case tr@TypeTree() => tr.original } -> body
    }

    val parts = builders.collect{
      case (name, builder) if parents.exists(_ selectsSome name) => builder.build(body, None)
    }.flatten

    c.Expr(q""" {
      println(${parts.map(showRaw(_)) mkString "\n"})
      null
    }""")
  }
}

trait ConstructionPart
trait AgentPart extends ConstructionPart
trait ControllerPart extends ConstructionPart
case class TestPart[C <: whitebox.Context](name: String, tree: C#Tree) extends ConstructionPart
case class ExtDefOverride[C <: whitebox.Context](name: String, extPoint: String, upd: C#Tree, forNegotiation: Option[C#Tree]) extends AgentPart

abstract class AgentSpecificationBuilder[C <: whitebox.Context](val c: C){
  lazy val h = new Helper[c.type](c)
  implicit def hWrapper(t: c.Tree) = new h.Wrapper(t)

  def build(tr: List[c.Tree], negotiationDef: Option[c.Tree]): List[ConstructionPart]
}

class PriorityAndProposalBasedBuilder[C <: whitebox.Context](_c: C) extends AgentSpecificationBuilder[C](_c) {
  import c.universe._

  def build(tr: List[c.Tree], negotiationDef: Option[c.Tree]): List[ConstructionPart] = tr.collect{
    case Apply(sel, List(arg)) if sel selectsSome "$anon.ExtendableMonoDefinitionWrapper" => extendableMonoDefinitionWrapper(sel, arg, negotiationDef)
    case Apply(Apply(AnonTypeApply("after"), List(after)), List(arg)) => extendableAfter(after, arg, negotiationDef)
    case Apply(TypeApply(sel, List(TypeTree())), args) if sel selectsSome "$anon.negotiationDefHasForeach" => forNegotiation(sel, args)
    case x3 => TestPart("todo", x3) :: Nil
  }.flatten

  def extendableMonoDefinitionWrapper(tree: c.Tree, body: c.Tree, negotiationDef: Option[c.Tree]) = tree match{
    case Select(
          Apply(
            AnonTypeApply("ExtendableMonoDefinitionWrapper"),
            List(AnonSelect(fname))
            ),
          TermName(op)
          )  =>

      op match {
        case "$colon$eq" =>
          ExtDefOverride(fname, "def", process(body), negotiationDef) :: Nil
      }
    case fail => c.abort(NoPosition, showRaw(fail))
  }

  def extendableAfter(after: c.Tree, body: c.Tree, negotiationDef: Option[c.Tree]) = after match {
    case AnonSelect(name) => ExtDefOverride(name, "after", body, negotiationDef) :: Nil
  }

  def forNegotiation(selTree: c.Tree, args: List[c.Tree]) = selTree match {
    case Select(
          Apply(
            AnonSelect("negotiationDefHasForeach"),
            List(negDef)
            ),
          TermName("foreach")
        ) =>
      args match{
        case List(
              Function(
                List(ValDef(_, neg, TypeTree(), EmptyTree)),
                body
              )
            ) =>
          body match{
            case a: Apply => build(a :: Nil, Some(negDef))
            case Block(block, last) => build(block :+ last, Some(negDef))
          }
        case _ => c.abort(c.enclosingPosition, "!!!1!!!" + showRaw(args))
      }
    case _ => c.abort(c.enclosingPosition, showRaw(selTree))
  }

  protected def process(tr: c.Tree) = tr.transform{
    case AnonSelect("log") => Select(This(TypeName("PriorityAndProposalBasedAgent")), TermName("log"))
  }

  object AnonSelect{
    def unapply(tree: c.Tree) = PartialFunction.condOpt(tree){
      case Select(This(TypeName("$anon")), TermName(name)) => name
    }
  }

  object AnonTypeApply{
    def unapply(tree: c.Tree) = PartialFunction.condOpt(tree){
      case TypeApply(AnonSelect(name), _) => name
    }
  }
}