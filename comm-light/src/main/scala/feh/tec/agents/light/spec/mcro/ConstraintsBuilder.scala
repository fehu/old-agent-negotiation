package feh.tec.agents.light.spec.mcro

import feh.tec.agents.light.spec.NegotiationSpecification
import feh.tec.agents.light.spec.NegotiationSpecification.{ConstraintPartLeaf, ConstraintPartComb, ConstraintPart}
import feh.tec.agents.light.spec.dsl.Negotiation
import scala.reflect.macros.whitebox

trait HasConstraintsBuilder[C <: whitebox.Context] extends NegotiationBuildingMacro[C] {

  trait ConstraintsBuilder{
    def build(agDef: Raw.AgentConstraintsDef, raw: NegotiationRaw): C#Expr[NegotiationSpecification.AgentConstraintsDef]
  }

}

trait HasSimpleConstraintsBuilder[C <: whitebox.Context] extends HasConstraintsBuilder[C] {

  class SimpleConstraintsBuilder extends ConstraintsBuilder{
    def build(dsl: C#Expr[Negotiation]): NegotiationRaw = ???

    def build(agDef: Raw.AgentConstraintsDef, raw: NegotiationRaw): C#Expr[NegotiationSpecification.AgentConstraintsDef] = {

      val xc = new ExtendedConstraint[C](c)
      import c.universe._

      val constraints = agDef.constraints map xc.extractConstraintsDef map {
        case (cName, xc.Replacement(descr, f)) =>
          //        val varsAndDomains = raw.vars.map()
          //        val func = f(raw.varsAndDomains.map(tr => tr._1.decodedName.toString.trim -> tr._2).toMap)
          val func = f(raw.vars.map{ case Raw.VarDef(name, Raw.DomainDef(_, tpe, _)) => name -> tpe }.toMap)
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

      val h = new Helper(c)
      import h._

      val xc = new ExtendedConstraint(c)

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
            val func = f(raw.vars.map{ case Raw.VarDef(name, Raw.DomainDef(_, tpe, _)) => name -> tpe }.toMap)
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

