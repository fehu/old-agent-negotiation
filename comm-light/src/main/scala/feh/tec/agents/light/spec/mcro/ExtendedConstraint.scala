package feh.tec.agents.light.spec.mcro

import scala.reflect.macros.whitebox

class ExtendedConstraint[C <: whitebox.Context](val c: C) {
  import c.universe._

  val h = new Helper[C](c)

  def extractConstraintsDef(t: C#Tree) = t match {
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


  def replace(in: C#Tree): Replacement = {

    def proposalArgName(s: String) = "$_proposal_" + s
    def proposalArg(s: String) = TermName(proposalArgName(s))
    
    def valueArgName(s: String) = "$_value_" + s
    def valueArg(s: String) = TermName(valueArgName(s))

    def param(name: TermName, tpe: C#Type) = ValDef(Modifiers(Flag.PARAM), name, Ident(tpe.typeSymbol.asInstanceOf[c.Symbol]), EmptyTree)

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

    def func(types: Map[String, C#Type]) = Function(
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
  case class Replacement(description: List[Description], build: Map[String, C#Type] => C#Tree)
}

