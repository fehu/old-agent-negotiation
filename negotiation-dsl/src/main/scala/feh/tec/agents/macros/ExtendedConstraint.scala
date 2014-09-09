package feh.tec.agents.macros

import scala.reflect.macros.whitebox
import scala.language.experimental.macros


class ExtendedConstraint[C <: whitebox.Context](val c: C) {

  def replace(in: c.Tree): Replaced = {
    import c.universe._

    val h = new Helper[c.type](c)


    
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

/*    def func_build(types: Map[c.Name, c.Type]) = Function(
      args.toList map {
        case (_, _, arg) => param(arg, types(arg))
      },
      transformed
    )*/

    def func = Function(
      args.toList map {
        case (_, _, arg) => param(arg, c.typeOf[Any])
      },
      transformed
    )

    val descr = args.toList map (Description.apply _).tupled

    Replaced(descr, func)
  }

  case class Description(tpe: String, varName: String, arg: c.TermName)
                                                               // Map[arg, arg's type]
//  case class Replacement(description: List[Description], build: Map[c.Name, c.Type] => c.Tree)
  case class Replaced(description: List[Description], tree: c.Tree)
}

