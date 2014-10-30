package feh.tec.agents.light.spec.macros

import feh.tec.agents.light.{NegotiationId, spec}
import feh.tec.agents.light.spec.NegotiationSpecification.NegotiationDef
import scala.reflect.macros.whitebox
import feh.util._

object AgentSpecificationBuilder {
  def build(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Agent]): c.Expr[spec.AgentSpecification] = {
    import c.universe._

    val helper = new Helper[c.type](c)
    import helper.Wrapper

    val syntaxBuilder = new AgentSpecificationSyntaxBuilder[c.type](c)

    val transformations = new AgentSpecificationSyntaxTransformations[c.type](c){
      val sb: AgentSpecificationSyntaxBuilder[c.type] = syntaxBuilder
      val h: Helper[c.type] = helper
    }

    lazy val SpecName = "feh.tec.agents.light.spec.dsl.Agent"

    val builders = Map(
      SpecName -> syntaxBuilder,
      "feh.tec.agents.light.spec.dsl.PriorityAndProposalBased" -> new PriorityAndProposalBasedBuilder[c.type](c)
    )

    val Some((parents, body)) = dsl.tree extractOne {
      case ClassDef(_, TypeName("$anon"), _, Template(parents, `noSelfType`, body)) if parents.exists(_ selects SpecName) =>
        parents.collect{ case tr@TypeTree() => tr.original } -> body
    }

    val parts = builders.collect{
      case (SpecName, builder) => builder.build(body, None)
      case (name, builder) if parents.exists(_ selectsSome name) => builder.build(body, None)
    }.flatten.map{
      case ext: ExtDefOverride[_] => transformations.transform(ext.asInstanceOf[ExtDefOverride[c.type]])
      case other => other
    }
//    println(${parts.map(showRaw(_)) mkString "\n"})

    c.Expr(q""" {
      println(${parts mkString "\n\n"})
      null
    }""")
  }
}

trait ConstructionPart
trait AgentPart extends ConstructionPart
trait ControllerPart extends ConstructionPart
case class TestPart[C <: whitebox.Context](name: String, tree: C#Tree) extends ConstructionPart
case class ExtDefOverride[C <: whitebox.Context](name: String, extPoint: String, upd: ExtDef, forNegotiation: Option[C#Expr[NegotiationDef]]) extends AgentPart

sealed trait ExtDef{
  def isRaw: Boolean
  final def isOwnerDependent = !isRaw
}
case class RawExtDef[C <: whitebox.Context](raw: C#Tree) extends ExtDef { final def isRaw = true }
case class OwnerDependent[U <: scala.reflect.macros.Universe](bodyByOwnerTerm: U#Ident => U#Tree) extends ExtDef { final def isRaw = false }

object ExtDef{
  implicit class OpsWrapper(extDef: ExtDef){
    def ops[C <: whitebox.Context](c: C)(implicit h: Helper[c.type]) = new Ops[c.type](c, extDef)
  }
  
  class Ops[C <: whitebox.Context](val c: C, extDef: ExtDef){
    def toEither = extDef match {
      case raw: RawExtDef[c.type] => Left(raw)
      case owd: OwnerDependent[c.universe.type] => Right(owd)
    }
    
    def compose(that: ExtDef) = ExtDef.compose[c.type](c, extDef, that)
    
    def map(f: c.Tree => c.Tree): ExtDef = mapInner(f)((fn, tr) => fn(tr))
    def transform(f: PartialFunction[c.Tree, c.Tree])(implicit h: Helper[c.type]): ExtDef =
      mapInner(f)((fn, tree) => h.transform(tree, fn))

    def flatMap(f: c.Tree => ExtDef): ExtDef = flatMapInner(f)((fn, tr) => fn(tr))
    def flatTransform(f: PartialFunction[c.Tree, ExtDef])(implicit h: Helper[c.type]): ExtDef = {
      var ownerDependentFlag = false
      val ownerDependent = flatMapInner(f){
          (fn, tr) =>
            OwnerDependent[c.universe.type](ow =>
              h.transform(tr, fn.andThen{
                case RawExtDef(raw)      => raw.asInstanceOf[c.Tree]
                case OwnerDependent(owd) =>
                  ownerDependentFlag = true
                  owd(ow).asInstanceOf[c.Tree]
              })
            )
        }.asInstanceOf[OwnerDependent[c.universe.type]]
      if(ownerDependentFlag) ownerDependent
      else RawExtDef(ownerDependent.bodyByOwnerTerm(c.universe.Ident(c.universe.termNames.WILDCARD)))
    }

    protected def mapInner[F <: c.Tree => c.Tree](f: F)(applyMap: (F, c.Tree) => c.Tree) = extDef match {
      case RawExtDef(raw)       => RawExtDef(applyMap(f, raw.asInstanceOf[c.Tree]))
      case OwnerDependent(owd)  => OwnerDependent[c.universe.type](ow => applyMap(f, owd(ow).asInstanceOf[c.Tree]))
    }

    protected def flatMapInner[F <: c.Tree => ExtDef](f: F)(applyMap: (F, c.Tree) => ExtDef) = extDef match {
      case RawExtDef(raw)       => applyMap(f, raw.asInstanceOf[c.Tree])
      case OwnerDependent(owd)  => OwnerDependent[c.universe.type](ow =>
                                                                     (applyMap(f, owd(ow).asInstanceOf[c.Tree]) match {
                                                                        case RawExtDef(raw)     => raw
                                                                        case OwnerDependent(fn) => fn(ow)
                                                                      }).asInstanceOf[c.Tree]
                                                                    )
    }
  }

  def compose[C <: whitebox.Context](c: C, ext1: ExtDef, ext2: ExtDef) = (ext1, ext2) match {
    case (RawExtDef(raw1), RawExtDef(raw2))           => composeRaws[c.type]          (c)(raw1.asInstanceOf[c.Tree], raw2.asInstanceOf[c.Tree])
    case (RawExtDef(raw), OwnerDependent(owd))        => composeMixed[c.type]         (c)(raw.asInstanceOf[c.Tree], owd.asInstanceOf[c.universe.Ident => c.Tree])
    case (OwnerDependent(owd), RawExtDef(raw))        => composeMixed[c.type]         (c)(raw.asInstanceOf[c.Tree], owd.asInstanceOf[c.universe.Ident => c.Tree])
    case (OwnerDependent(owd1), OwnerDependent(owd2)) => composeOwnerDependent[c.type](c)(owd1.asInstanceOf[c.universe.Ident => c.Tree], owd2.asInstanceOf[c.universe.Ident => c.Tree])
  }

  protected def composeRaws[C <: whitebox.Context](c: C)(raw1: c.Tree, raw2: c.Tree) =
    RawExtDef[c.type](ifOneIsBlockCombine[c.type](c)(raw1, raw2))

  protected def composeMixed[C <: whitebox.Context](c: C)(raw: c.Tree, owd: c.universe.Ident => c.Tree) =
    OwnerDependent[c.universe.type](ow => ifOneIsBlockCombine[c.type](c)(raw, owd(ow)))

  protected def composeOwnerDependent[C <: whitebox.Context](c: C)(owd1: c.universe.Ident => c.Tree, owd2: c.universe.Ident => c.Tree) =
    OwnerDependent[c.universe.type](ow => ifOneIsBlockCombine[c.type](c)(owd1(ow), owd2(ow)))

  private def ifOneIsBlockCombine[C <: whitebox.Context](c: C)(tr1: c.Tree, tr2: c.Tree) = {
    import c.universe._

    def isBlock(tr: c.Tree) = PartialFunction.condOpt(tr){
      case Block(body, expr) => body -> expr
    }
    isBlock(tr1).map{ case (body, last) => Block(body :+ last, tr2)} orElse
      isBlock(tr2).map{ case (body, last) => Block(tr2 +: body, last)} getOrElse
        Block(tr1 :: Nil, tr2)
  }
}

object OwnerDependent {

  implicit class OpsWrapper(owd: OwnerDependent[_]){
    def ops[C <: whitebox.Context](c: C) = new Ops[c.type](c, owd)
  }

  class Ops[C <: whitebox.Context](val c: C, owd: OwnerDependent[_]) {
    import c.universe._

    def body(ownerTerm: TermName) = owd.asInstanceOf[OwnerDependent[c.universe.type]].bodyByOwnerTerm(Ident(ownerTerm))

    def toFunction(ownerTerm: TermName) = Function(
      ValDef(Modifiers(Flag.PARAM), ownerTerm, TypeTree(), EmptyTree) :: Nil,
      body(ownerTerm)
    )
  }
}

abstract class AgentSpecificationBuilder[C <: whitebox.Context](val c: C){
  lazy val h = new Helper[c.type](c)
//  implicit def hWrapper(t: c.Tree) = new h.Wrapper(t)

  def build(tr: List[c.Tree], negotiationDef: Option[c.Expr[NegotiationDef]]): List[ConstructionPart]
}

class AgentSpecificationSyntaxBuilder[C <: whitebox.Context](_c: C) extends AgentSpecificationBuilder[C](_c) {
  import c.universe._
  import h._

  def build(tr: List[c.Tree], negotiationDef: Option[c.Expr[NegotiationDef]]): List[ConstructionPart] = tr.collect{
    case Apply(sel, List(arg)) if sel selectsSome "$anon.ExtendableMonoDefinitionWrapper" => extendableMonoDefinitionWrapper(sel, arg, negotiationDef)
    case Apply(Apply(AnonTypeApply("after"), List(after)), List(arg)) => extendableAfter(after, arg, negotiationDef)
    case Apply(TypeApply(sel, List(TypeTree())), args) if sel selectsSome "$anon.negotiationDefHasForeach" => forNegotiation(sel, args)
//    case x3 => TestPart("todo", x3) :: Nil
  }.flatten

/*
  case class OwnerDependent(bodyByOwnerTerm: Ident => c.Tree){
    def body(ownerTerm: TermName) = bodyByOwnerTerm(Ident(ownerTerm))
    def toFunction(ownerTerm: TermName) = Function(
      ValDef(Modifiers(Flag.PARAM), ownerTerm, TypeTree(), EmptyTree) :: Nil,
      body(ownerTerm)
    )
  }
*/

  protected def extendableMonoDefinitionWrapper(tree: c.Tree, body: c.Tree, negotiationDef: Option[c.Expr[NegotiationDef]]) = tree match{
    case Select(
          Apply(
            AnonTypeApply("ExtendableMonoDefinitionWrapper"),
            List(AnonSelect(fname))
            ),
          TermName(op)
          )  =>

      op match {
        case "$colon$eq" => ExtDefOverride(fname, "def", RawExtDef(body), negotiationDef) :: Nil
      }
    case fail => c.abort(NoPosition, showRaw(fail))
  }

  protected def extendableAfter(after: c.Tree, body: c.Tree, negotiationDef: Option[c.Expr[NegotiationDef]]) = after match {
    case AnonSelect(name) => ExtDefOverride(name, "after", RawExtDef(body), negotiationDef) :: Nil
  }

  protected def forNegotiation(selTree: c.Tree, args: List[c.Tree]) = selTree match {
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
            case a: Apply => build(a :: Nil, Some(c.Expr(negDef)))
            case Block(block, last) => build(block :+ last, Some(c.Expr(negDef)))
          }
        case _ => c.abort(c.enclosingPosition, "!!!1!!!" + showRaw(args))
      }
    case _ => c.abort(c.enclosingPosition, showRaw(selTree))
  }

//  protected def process(tr: c.Tree) = tr.transform{
//    case AnonSelect("log") => TermName("owner") |> {
//      ow => ownerExtFunc(ow, Select(Ident(ow), TermName("log")))
//    }
//  }
}

abstract class AgentSpecificationSyntaxTransformations[C <: whitebox.Context](val c: C){
  import c.universe._
  implicit def context = c

  val sb: AgentSpecificationSyntaxBuilder[c.type]

  implicit val h: Helper[c.type]
  import h._

  lazy val negIdByDef: c.Expr[NegotiationDef] => c.Expr[NegotiationId] =
    neg => c.Expr(q"""NegotiationId($neg.name)""")

  lazy val forAllNegotiations: (c.Expr[NegotiationId] => OwnerDependent[c.universe.type]) => OwnerDependent[c.universe.type] =
    f => {
      def fTree: c.Expr[Any/*owner*/ => NegotiationId => c.Tree] = c.Expr(q"(ow => {id => ${
        f(c.Expr(Ident(TermName("id")))).ops[c.type](c).toFunction(TermName("ow"))
      }(ow) } )")

      OwnerDependent(ow => q"""$ow.negotiations.map($fTree($ow))""")
    }

  def transform: ExtDefOverride[c.type] => ExtDefOverride[c.type] = {
//    case ext@ExtDefOverride(name, extPoint, tree, negOpt) if containsDslCalls(tree.asInstanceOf[c.Tree]) =>
//      ext.copy[c.type](upd = applyTransforms(ext))
    case ext => ext.copy[c.type](upd = applyTransforms(ext))
  }

  def containsDslCalls(tree: c.Tree) = tree selectsSome "$anon"

  case class Transform(get: ExtDefOverride[c.type] => PartialFunction[c.Tree, ExtDef])

  protected val transforms = transform_state_eq :: Nil



  protected lazy val transform_state_eq = Transform(ext => {
    case Apply(AnonSelect("state_$eq"), List(arg)) => 
      def f = (id: c.Expr[NegotiationId]) => OwnerDependent[c.universe.type](ow => q"""$ow.get($id).currentState.update($arg)""")

      ext.forNegotiation.map(x => f compose negIdByDef apply x.asInstanceOf[c.Expr[NegotiationDef]])
        .getOrElse(forAllNegotiations(f))
  })

  protected def applyTransforms(xdo: ExtDefOverride[c.type]) = xdo match {
    case ExtDefOverride(name, extPoint, upd, negOpt) =>
      (upd /: transforms) {
        case (defAcc, Transform(treeTransform)) => defAcc.ops[c.type](c).flatTransform(treeTransform(xdo))
      }
      
//    case ExtDefOverride(name, extPoint, RawExtDef(tree), negOpt) =>
//    case ExtDefOverride(name, extPoint, OwnerDependent(tree), negOpt) =>
  }

//    (ext: ExtDefOverride[c.type]) => (tr /: transforms){
//    case (acc, Transform(replace)) =>
//      val q = replace(ext)
//      acc transform q
//  }
}

class PriorityAndProposalBasedBuilder[C <: whitebox.Context](_c: C)
  extends AgentSpecificationBuilder[C](_c){
  import c.universe._

  def build(tr: List[c.Tree], negotiationDef: Option[c.Expr[NegotiationDef]]): List[ConstructionPart] = Nil // todo
}
