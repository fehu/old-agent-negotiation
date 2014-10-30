package feh.tec.agents.light.spec.macros

import scala.reflect.macros.whitebox
import feh.util._

class Helper[C <: whitebox.Context](protected val c: C){
  helper =>

  import c.universe._

  def transform(t: c.Tree, f: PartialFunction[c.Tree, c.Tree]): c.Tree = {

    def transformIn = transform(_: c.Tree, f)
    t match {
      case tree if f isDefinedAt tree => f(tree)
      case Function(params, body) => Function(params, transformIn(body))
      case Select(qual, name)     => Select(transformIn(qual), name)
      case Apply(fun, args)       => Apply(transformIn(fun), args.map(transformIn))
      case TypeApply(fun, args)   => TypeApply(transformIn(fun), args.map(transformIn))
      case Block(body, last)      => Block(body.map(transformIn), transformIn(last))
      case other => other
    }
  }


  def extract[R](from: c.Tree, f: PartialFunction[c.Tree, R]): List[R] = {
    def extractFrom = extract(_: c.Tree, f)

    from match {
      case tree if f isDefinedAt tree => f(tree) :: Nil
      case Function(params, body) => extractFrom(body) ::: params.flatMap(extractFrom)
      case Select(qual, _)        => extractFrom(qual)
      case Apply(fun, args)       => extractFrom(fun) ::: args.flatMap(extractFrom)
      case TypeApply(fun, args)   => extractFrom(fun) ::: args.flatMap(extractFrom)
      case Block(body, last)      => body.flatMap(extractFrom) ::: extractFrom(last)
      case other => Nil
    }
  }

  def extractOne[R](from: c.Tree, f: PartialFunction[c.Tree, R]): Option[R] = extract(from, f) match {
    case e :: Nil => Some(e)
    case _ => None
  }

  def selects(in: c.Tree, what: String): Boolean = selectsInner(in, what){
    rec => (tree, path) =>
      PartialFunction.cond(path, tree){
        case (p :: tail, Select(next, name)) if p == name.decodedName.toString => rec(next, tail)
        case (p :: tail, Select(next, name)) if name == typeNames.PACKAGE && p == "package"  => rec(next, tail)
//        case (p :: "this" :: Nil, This(name)) if p == name.decodedName.toString => true
        case (p :: Nil, Ident(name)) if p == name.decodedName.toString => true
        case (p :: Nil, This(name)) if p == name.decodedName.toString => true
      }
  }

  def selectsSome(in: c.Tree, what: String) = selectsInner(in, what){
    rec => (tree, path) =>
      PartialFunction.cond(path, tree){
        case (p :: tail, Select(next, name)) if p == name.decodedName.toString => rec(next, tail)
        case (p :: tail, Select(next, name)) if name == typeNames.PACKAGE && p == "package"  => rec(next, tail)
        case (p, Select(next, _)) => rec(next, p)
        case (p, Apply(fun, args))     => rec(fun, p) || args.exists(rec(_, p))
        case (p, TypeApply(fun, args)) => rec(fun, p) || args.exists(rec(_, p))
        case (p :: Nil, Ident(name)) if p == name.decodedName.toString => true
//        case (p :: "this" :: Nil, This(name)) if p == name.decodedName.toString => true
        case (p :: Nil, This(name)) if p == name.decodedName.toString => true
      }
  }

  protected def selectsInner(in: c.Tree, what: String)
                            (isTheOneSearched_? : Y2[c.Tree, Seq[String], Boolean]): Boolean = {
    val w = what.split('.').reverse.toList

    def search(sel: c.Tree) = isTheOneSearched_?(Y2(isTheOneSearched_?))(sel, w)

    def rec(t: c.Tree): Boolean =
      PartialFunction.cond(t) {
        case Function(_, body)    => rec(body)
        case Apply(fun, args)     => rec(fun) || args.exists(rec)
        case TypeApply(fun, args) => rec(fun) || args.exists(rec)
        case tr@TypeTree()        => rec(tr.original)
        case sel                  => search(sel)
      }
    rec(in)
  }

  implicit class Wrapper(t: c.Tree){
    def transform(f: PartialFunction[c.Tree, c.Tree]) = helper.transform(t, f)
    def extract[R](f: PartialFunction[c.Tree, R]) = helper.extract(t, f)
    def extractOne[R](f: PartialFunction[c.Tree, R]) = helper.extractOne(t, f)
    def selects(fullName: String) = helper.selects(t, fullName)
    def selectsSome(fullName: String) = helper.selectsSome(t, fullName)
//    def selectsInBetween(fullName: String) = helper.selectsInBetween(t, fullName)
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