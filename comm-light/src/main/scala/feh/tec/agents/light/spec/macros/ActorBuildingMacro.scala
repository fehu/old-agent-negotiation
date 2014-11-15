package feh.tec.agents.light.spec.macros

import akka.actor.Props
import scala.reflect.macros.whitebox

trait ActorBuildingMacro[C <: whitebox.Context] extends MacroContext[C]{
  type CreatePropsArgs = Map[String, Any]

  def actorCreatePropsExpr(trees: ActorTrees): c.Expr[CreatePropsArgs => Props]

  /** Trees container for building actors
   * @param parents actor parent types
   * @param body actor body definitions
   * @param constructorArgs named arguments for actor's constructor
   */
  case class ActorTrees(className: String, parents: List[c.Type], body: List[c.Tree], constructorArgs: Map[String, c.Type]) {
    aBuilder =>
    
    object add{
      def constructorArgs(a: (String, c.Type)*) = copy(constructorArgs = aBuilder.constructorArgs ++ a.toMap)
    }

    object append{
      def parents(p: c.Type*) = copy(parents = aBuilder.parents ::: p.toList)
      def body(b: c.Tree*) = copy(body = aBuilder.body ::: b.toList.flatMap(disarmBlock))
    }

    object prepend{
      def parents(p: c.Type*) = copy(parents = p.toList ::: aBuilder.parents)
      def body(b: c.Tree*) = copy(body = b.toList.flatMap(disarmBlock) ::: aBuilder.body)
    }

    private def disarmBlock: (c.Tree => List[c.Tree]) = {
      import c.universe._
      {
        case q"{..$block}" => block
        case other => other :: Nil
      }
    }
  }
}

trait ActorBuildingMacroImpl[C <: whitebox.Context] extends ActorBuildingMacro[C]{
  def actorCreatePropsExpr(trees: ActorTrees): c.Expr[CreatePropsArgs => Props] = {
    import c.universe._

    val (pClasses, pTraits) = trees.parents.partition(_.decls.exists(_.name == termNames.CONSTRUCTOR))
    val pTraitsTrees = pTraits.map(_.asInstanceOf[c.Type]).map(TypeTree(_))
    val pClassOpt = pClasses.ensuring(pClasses.size <= 1).headOption

    val (extendsName, extendsTArgs, extendsArgs, withStatements) = pClassOpt map {
      pClass =>
        val nme = pClass.typeSymbol.asType.asInstanceOf[c.universe.TypeSymbol] //.name.asInstanceOf[c.TypeName]
        val args = pClass.decl(termNames.CONSTRUCTOR).asMethod.paramLists.ensuring(_.size == 1).head
          .map(s => Ident(s.name.asInstanceOf[c.Name]))
        val targs = pClass.typeArgs.map(_.asInstanceOf[c.Type])
        (nme, targs, args, pTraitsTrees)
      } getOrElse {
          val h = pTraits.head
          (h.typeSymbol.asType.asInstanceOf[c.universe.TypeSymbol], h.typeArgs.map(_.asInstanceOf[c.Type]), Nil, pTraitsTrees.tail)
      }


    val paramStatements = trees.constructorArgs.map {
      case (name, tpe) => ValDef(Modifiers(Flag.PARAM), TermName(name), TypeTree(tpe.asInstanceOf[c.Type]), EmptyTree)
    }

    val classDef = q"""
      class ${TypeName(trees.className)}(..$paramStatements)
        extends $extendsName[..$extendsTArgs](..$extendsArgs) with ..$withStatements
      {
        ..${trees.body.map(_.asInstanceOf[c.Tree])}
      }
    """

    def classInstance(argsExpr: c.Expr[Map[String, Any]]) = {
      val namedArgs = trees.constructorArgs.map{
        case (name, tpe) =>
          AssignOrNamedArg(Ident(TermName(name)), q"$argsExpr($name).asInstanceOf[${tpe.asInstanceOf[c.Type]}]")
      }
      q"new ${TypeName(trees.className)}(..$namedArgs)"
    }

    def propsTree(args: c.Expr[CreatePropsArgs]) = q"""
      $classDef
      akka.actor.Props(${classInstance(args)})
    """

    c.Expr[CreatePropsArgs => Props](q"""(createPropsArgs: Map[String, Any]) => ${propsTree(c.Expr(Ident(TermName("createPropsArgs"))))} """)
  }
}