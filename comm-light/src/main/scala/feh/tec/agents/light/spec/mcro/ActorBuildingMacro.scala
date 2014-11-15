package feh.tec.agents.light.spec.mcro

import akka.actor.Props
import scala.reflect.macros.whitebox

trait ActorBuildingMacro[C <: whitebox.Context] extends MacroContext[C]{
  type CreatePropsArgs = Map[String, Any]

  def actorCreatePropsExpr(trees: ActorTrees): C#Expr[CreatePropsArgs => Props]

  /** Trees container for building actors
   * @param parents actor parent types
   * @param body actor body definitions
   * @param constructorArgs named arguments for actor's constructor
   */
  case class ActorTrees(className: String, parents: List[C#Type], body: List[C#Tree], constructorArgs: Map[String, C#Type]) {
    aBuilder =>
    
    object add{
      def constructorArgs(a: (String, C#Type)*) = copy(constructorArgs = aBuilder.constructorArgs ++ a.toMap)
    }

    object append{
      def parents(p: C#Type*) = copy(parents = aBuilder.parents ::: p.toList)
      def body(b: C#Tree*) = copy(body = aBuilder.body ::: b.toList.flatMap(disarmBlock))
    }

    object prepend{
      def parents(p: C#Type*) = copy(parents = p.toList ::: aBuilder.parents)
      def body(b: C#Tree*) = copy(body = b.toList.flatMap(disarmBlock) ::: aBuilder.body)
    }

    private def disarmBlock: (C#Tree => List[C#Tree]) = {
      import c.universe._
      {
        case q"{..$block}" => block
        case other => other :: Nil
      }
    }
  }
}

trait ActorBuildingMacroImpl[C <: whitebox.Context] extends ActorBuildingMacro[C]{
  def actorCreatePropsExpr(trees: ActorTrees): C#Expr[CreatePropsArgs => Props] = {
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

    val classDef2 = q"""
      class ${TypeName(trees.className)}(..$paramStatements)
        extends $extendsName[..$extendsTArgs](..$extendsArgs) with ..$withStatements
      {
        ..${trees.body.map(_.asInstanceOf[c.Tree])}
      }
    """

//    c.info(NoPosition, "#classDef2 = "showCode(classDef2), true)

    def classInstance2(argsExpr: c.Expr[Map[String, Any]]) = {
      val namedArgs = trees.constructorArgs.map{
        case (name, tpe) =>
          AssignOrNamedArg(Ident(TermName(name)), q"$argsExpr($name).asInstanceOf[${tpe.asInstanceOf[c.Type]}]")
      }
      q"new ${TypeName(trees.className)}(..$namedArgs)"
    }

    def propsTree2(args: c.Expr[CreatePropsArgs]) = q"""
      $classDef2
      akka.actor.Props(${classInstance2(args)})
    """

    /* ** ** OLD ** ** */

   /*
    val supConstructorOpt = trees.parents
      .flatMap(_.decls.withFilter(_.name == termNames.CONSTRUCTOR).map(_.asMethod)).ensuring(_.size <= 1).headOption
    val supConstructorSignatureOpt = supConstructorOpt map(
      _.paramLists.map(_.map(param => param.name.decodedName.toString -> param.typeSignature))
      )

    def constructor(params: List[(String, c.Type)], superSign: List[(String, c.Type)]) = {
      assert(superSign.forall(params.contains))
      DefDef(
        Modifiers(),
        termNames.CONSTRUCTOR,
        List(),
        params.map {
          case (name, tpe) => ValDef(Modifiers(Flag.PARAM), TermName(name), TypeTree(tpe), EmptyTree)
        } :: Nil,
        TypeTree(),
        pendingSuperCall
/*
        Block(
          Apply(
            Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR),
            superSign.map { case (name, _) => Ident(TermName(name)) }
          ) :: Nil,
          Literal(Constant(()))
        )
*/
      )
    }

    val constructorTree = {
      val supParams = supConstructorSignatureOpt.getOrElse(List(Nil)).ensuring(_.size == 1).head.asInstanceOf[List[(String, c.Type)]]
      constructor(trees.constructorArgs.mapValues(_.asInstanceOf[c.Type]).toList, supParams)
    }

    val anonClassName = TypeName(trees.className)

    val classDef =
      ClassDef(Modifiers(), anonClassName, Nil, Template(
        trees.parents.map(_.asInstanceOf[c.Type]).map(TypeTree(_)),
        noSelfType,
        constructorTree :: trees.body.map(_.asInstanceOf[c.Tree])
      )
    )

    def classInstance(argsExpr: c.Expr[Map[String, Any]]) = {
      val namedArgs = trees.constructorArgs.map{
        case (name, tpe) =>
          AssignOrNamedArg(Ident(TermName(name)), q"""$argsExpr($name).ensuring(_.isInstanceOf[${tpe.asInstanceOf[c.Type]}])""")
      }
      q"new $anonClassName(..$namedArgs) {}"
    }

    def propsTree(args: c.Expr[CreatePropsArgs]) = q"""
      $classDef
      akka.actor.Props(${classInstance(args)})
    """*/

//    c.abort(NoPosition,
//      s"""constructorOpt = ${showRaw(supConstructorOpt)}
//         |constructorSignatureOpt = ${showRaw(supConstructorSignatureOpt)}
//         |constructorTree = ${showRaw(constructorTree)}
//         |classDef = ${showRaw(classDef)}
//       """.stripMargin )

    c.Expr[CreatePropsArgs => Props](q"""(createPropsArgs: Map[String, Any]) => ${propsTree2(c.Expr(Ident(TermName("createPropsArgs"))))} """)
  }
}