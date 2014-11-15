package feh.tec.agents.light.spec.macros

import scala.reflect.macros.whitebox

class ExtendedConstraint[C <: whitebox.Context](val c: C) {
  import c.universe._

  val h = new Helper[c.type](c)

  def extractConstraintsDef(t: c.Tree) = t match {
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
        ) => name -> replace(constraint)
  }


  def replace(in: c.Tree): Replacement = {

    def proposalArgName(s: String) = "$_proposal_" + s
    def proposalArg(s: String) = TermName(proposalArgName(s))
    
    def valueArgName(s: String) = "$_value_" + s
    def valueArg(s: String) = TermName(valueArgName(s))

    def param(name: TermName, tpe: c.Type) = ValDef(Modifiers(Flag.PARAM), name, Ident(tpe.typeSymbol), EmptyTree)

    var args: Seq[(String, String, TermName)] = Nil

    val transformed = h.transform(in, {
      case Apply(
            TypeApply(Select(This(TypeName("$anon")), TermName(typeOfArg)), List(TypeTree())),
            List(Select(This(TypeName("$anon")), TermName(term)))
          ) =>
        typeOfArg match{
          case "proposed" =>
            val arg = proposalArg(term)
            args :+= ("proposed", term, arg)
            Ident(arg)
          case "valueOf"  =>
            val arg = valueArg(term)
            args :+= ("valueOf", term, arg)
            Ident(arg)
        }
    })

    def func(types: Map[String, c.Type]) = Function(
      args.toList map {
        case (_, name, arg) => param(arg, types(name))
      },
      transformed.asInstanceOf[c.Tree]
    )

    val descr = args.toList map (Description.apply _).tupled

    Replacement(descr, func)
  }

  case class Description(tpe: String, varName: String, arg: C#TermName)
                                                             // Map[var name, var's type]
  case class Replacement(description: List[Description], build: Map[String, c.Type] => c.Tree)
}

