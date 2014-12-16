package feh.tec.agents.lite.spec.macros.impl

import feh.tec.agents.lite.spec.NegotiationSpecification
import feh.tec.agents.lite.spec.NegotiationSpecification.{ConstraintPart, ConstraintPartComb, ConstraintPartLeaf}
import feh.tec.agents.lite.spec.dsl.Negotiation
import feh.tec.agents.lite.spec.macros.{Helper, ExtendedConstraint, HasConstraintsBuilder}
import scala.reflect.macros.whitebox


trait HasSimpleConstraintsBuilder[C <: whitebox.Context] extends HasConstraintsBuilder[C] {

  class SimpleConstraintsBuilder extends ConstraintsBuilder{
    def build(dsl: c.Expr[Negotiation]): NegotiationRaw = ???

    def build(agDef: Raw.AgentConstraintsDef, raw: NegotiationRaw): c.Expr[NegotiationSpecification.AgentConstraintsDef] = {

      val xc = new ExtendedConstraint[c.type](c)
      import c.universe._

      val constraints = agDef.constraints map xc.extractConstraintsDef map {
        case (cName, xc.Replacement(descr, f)) =>
          val func = f(raw.vars.map{ case Raw.VarDef(name, Raw.DomainDef(_, tpe, _, _)) => name -> tpe }.toMap)
          val descriptions = descr map {
            case xc.Description(tpe, varName, arg) =>
              q"""ConstraintParamDescription($tpe, $varName, ${arg.decodedName.toString})"""
          }
          q"""AgentConstraintDef($cName, Seq(..$descriptions), ${func.asInstanceOf[c.Tree]}.tupled.asInstanceOf[Product => Boolean])"""
      }
      c.Expr(q"AgentConstraintsDef(Seq(..$constraints))")
    }
  }

}

trait HasVarsSeparatingConstraintsBuilder[C <: whitebox.Context] extends HasConstraintsBuilder[C]{


  class VarsSeparatingConstraintsBuilder extends ConstraintsBuilder{

    def build(agConstrDef: Raw.AgentConstraintsDef, raw: NegotiationRaw): c.Expr[NegotiationSpecification.AgentConstraintsDef] = {
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

      def separateAndOr(in: c.Tree): ConstraintPart[c.Tree] = in match {
        case Apply(Select(left, op@(TermName("$bar$bar") | TermName("$amp$amp"))), List(right)) =>
          ConstraintPartComb[c.Tree, c.TermName](op.asInstanceOf[c.TermName], separateAndOr(left), separateAndOr(right))
        case other if other.tpe == typeOf[Boolean] => ConstraintPartLeaf[c.Tree](other)
      }

      val separatedByName = agConstrDef.constraints.map{
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
      }.toMap

      val replacementsByName = separatedByName.mapValues(
        _.transform(xc.replace, (n: c.TermName) => n.toString)
      )

      val agentVarConstraintDefByName = replacementsByName.mapValues{
        _.transform(
        {
          case xc.Replacement(descr, f) =>
            val func = f(raw.vars.map{ case Raw.VarDef(name, Raw.DomainDef(_, tpe, _, _)) => name -> tpe }.toMap)
            c.info(NoPosition, "agentVarConstraintDefByName Replacement func" + showCode(func), true)
            val descriptions = descr map {
              case xc.Description(tpe, varName, arg) =>
                q"""ConstraintParamDescription($tpe, $varName, ${arg.decodedName.toString})"""
            }
            q"""AgentVarConstraintDef(Seq(..$descriptions), ${func.asInstanceOf[c.Tree]}.tupled.asInstanceOf[Product => Boolean])"""
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

