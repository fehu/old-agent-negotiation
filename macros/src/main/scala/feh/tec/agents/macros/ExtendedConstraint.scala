package feh.tec.agents.macros

import scala.reflect.macros.blackbox
import scala.language.experimental.macros
import feh.tec.agents.impl.ExtendedConstraintBuilder._


object ExtendedConstraint {
  def impl[T: c.WeakTypeTag](c: blackbox.Context)(withWrapper: c.Expr[CW[T] => Boolean]): c.Expr[(T, T) => Boolean] = {
    import c.universe._

    def dsl = withWrapper.tree

    val Function(List(ValDef(_, iParam, _, _)), body) = dsl
    val IParam = iParam


    def transformTree(t: c.Tree, f: PartialFunction[c.Tree, c.Tree]): c.Tree = {
      def transformIn = transformTree(_: c.Tree, f)
      t match {
        case tree if f isDefinedAt tree => f(tree)
        case Function(params, body) => Function(params, transformIn(body))
        case Select(qual, name) => Select(transformIn(qual), name)
        case Apply(fun, args) => Apply(transformIn(fun), args.map(transformIn))
        case TypeApply(fun, args) => TypeApply(transformIn(fun), args.map(transformIn))
        case other => other
      }
    }
    
    def proposalArgName = "$_proposal"
    def proposalArg = Ident(TermName(proposalArgName))
    
    def valueArgName = "$_value"
    def valueArg = Ident(TermName(valueArgName))
    
    def replaceCBuildKey: PartialFunction[c.Tree, c.Tree] = {
       case Select(Select(Select(Select(Select( Ident(
              TermName("feh")),
                TermName("tec") ),
                  TermName("agents")
                  ),TermName("impl")
                    ),TermName("ExtendedConstraintBuilder")
                      ),TermName("proposal")
                        ) =>
         proposalArg
       case Select(Select(Select(Select(Select( Ident(
              TermName("feh")
              ),TermName("tec")
                ),TermName("agents")
                  ),TermName("impl")
                    ),TermName("ExtendedConstraintBuilder")
                      ),TermName("value")
                        ) =>
         valueArg
    }

    val replacements = ({
      case  Apply(
              Select(
                Ident(IParam),
                TermName("apply")
              ),
              List(arg)
            ) => replaceCBuildKey.lift(arg).getOrElse(arg)
      case Apply(Apply( TypeApply(
            Select(Select(Select(Select(Select(Select( Ident(
              TermName("feh")),
                TermName("tec")
                ),TermName("agents")
                  ),TermName("impl")
                    ),TermName("ExtendedConstraintBuilder")
                      ),TermName("my")
                        ),TermName("current")
            ),List(TypeTree())
            ),List(Select(Select(Select(Select(Select(Ident(
              TermName("feh")),
                TermName("tec")
                ),TermName("agents")
                  ),TermName("impl")
                    ),TermName("ExtendedConstraintBuilder")
                      ),TermName("value")
            ))),List(Ident(IParam))
          ) =>
        valueArg
    }: PartialFunction[c.Tree, c.Tree]) orElse replaceCBuildKey


    def param(name: String) = ValDef(Modifiers(Flag.PARAM), TermName(name), Ident(c.weakTypeOf[T].typeSymbol), EmptyTree)

    val transformed = transformTree(body, replacements)

    val func = Function(
      List(param(proposalArgName), param(valueArgName)),
      transformed
    )

//
    val s = c.universe.showRaw(func)
//
//    c.Expr[(T, T) => Boolean]( q"""{println($s); ((t1, t2) => true)}""")
    c.Expr[(T, T) => Boolean](func)
  }


  class CW[T] protected[macros] (f: CBuilderKey => T) extends (CBuilderKey => T){
    def apply(v1: CBuilderKey) = f(v1)
  }

}

