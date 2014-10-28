package feh.tec.agents.light.spec.macros

import akka.actor.Props
import akka.util.Timeout
import feh.tec.agents.light.AgentCreationInterface.NegotiationInit
import feh.tec.agents.light.impl.NegotiationEnvironmentController
import feh.tec.agents.light.impl.NegotiationEnvironmentController.Timeouts
import feh.tec.agents.light.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.light.spec.AgentSpecification
import feh.tec.agents.light.spec.NegotiationSpecification.{AgentDef, NegotiationDef}
import feh.tec.agents.light.spec.macros.NegotiationSpecificationBuilder.{SimpleConstrainsBuilder, Raw}
import feh.tec.agents.light._
import feh.tec.agents.light.spec.macros.NegotiationSpecificationBuilder.Raw.{DomainDef, VarDef, AgentNegDef}
import feh.util._
import scala.reflect.macros.whitebox
import scala.concurrent.duration._

object NegotiationControllerBuilder {
  def build(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[Props] = {
    import c.universe._

    val builder = new NegotiationControllerBuilder[c.type](c)
    val raw = NegotiationSpecificationBuilder.raw[c.type](c)(dsl)
    implicit val cb = new SimpleConstrainsBuilder
    
    val specCompositionAndDependencies = raw.agents.zipMap{
      rawAgDef =>
        val comp = builder determineSpecificationComposition rawAgDef.spec.asInstanceOf[c.Expr[AgentSpecification]]
        comp -> builder.dependenciesWithCalls(comp)
    }.toMap

    val varExprByName = raw.variables.map(builder.buildVar).toMap
    val negotiations = raw.negotiations.map{ case NegotiationDef(name, issues) => name -> issues }.toMap

    val specCompositionsByRaw = specCompositionAndDependencies.mapValues(_._1)
    val dependenciesByRaw = specCompositionAndDependencies.mapValues(_._2).toMap
    val dependencyCallsByName = dependenciesByRaw.values.flatMap(_.mapValues(_._2)).toMap

    val createInterfacesPropsByBody = specCompositionsByRaw map(p => p._1 -> builder.agentPropsExpr(p._2, p._1, dependencyCallsByName)) map {
      case (agRawDef, agentPropsExpr) => agRawDef.name -> agentPropsExpr
    }

    def agentsCreationExpressions(name: String, body: List[c.Tree]): c.Expr[(String, NegotiationRole, Set[NegotiationInit]) => AgentRef] = c.Expr(
      q"""
        {case (uniqueName, role, negInits) => AgentRef(
          Agent.Id(uniqueName, role),
          implicitly[ActorSystem].actorOf(
            ${createInterfacesPropsByBody(name)(body)}(uniqueName, role, negInits))
          )}
      """
    )

    val timeoutExprByName = raw.timeouts.mp
    def timeoutOrDefault(name: String, default: => Timeout) = {
      val dur = timeoutExprByName.getOrElse(name, c.Expr(q"""FiniteDuration(${default.duration.toMillis}, "millis")"""))
      q"Timeout($dur)"
    }

    val timeouts =
      q"""
        import akka.util.Timeout
        new NegotiationEnvironmentController.Timeouts {
          lazy val initialize = ${timeoutOrDefault("initialize", TimeoutsDefault.initialize)}
          lazy val start      = ${timeoutOrDefault("start", TimeoutsDefault.start)}
          lazy val stop       = ${timeoutOrDefault("stop", TimeoutsDefault.stop)}
          lazy val reset      = ${timeoutOrDefault("reset", TimeoutsDefault.reset)}
        }
      """

    val spawns: c.Expr[Map[String, Int]] = c.Expr(q"${Raw.TreesBuilder.spawns[c.type](c)(raw.spawns)}.asInstanceOf[SimpleSpawnDef].mp")
    val issues: List[c.Expr[(String, Var)]] = varExprByName
      .map{ case (varName, varTree) => c.Expr[(String, Var)](q"$varName -> $varTree") }
      .toList
    val issuesByNegotiation: List[c.Expr[(String, Seq[String])]] = negotiations
      .map{ case (negName, iss) => c.Expr[(String, Seq[String])](q"$negName -> Seq(..$iss)") }
      .toList
    val initialAgents: List[c.Expr[(AgentDef, NegotiationEnvironmentController#CreateInterface => AgentRef)]] =
      specCompositionsByRaw.keySet.toList.map{
        rawAgDef =>
          val (agTpe, langTpe) = builder.typesOf(rawAgDef)

          val (createNegotiationTree, negotiationTypeTree) = builder.createNegotiationAndNegotiationTypeTree(agTpe)
//          c.abort(NoPosition, showRaw(q))

          c.info(NoPosition, showCode(createNegotiationTree), true)

          val body = negotiationTypeTree :: createNegotiationTree :: Nil

//          c.abort(NoPosition, showRaw(body))

          c.Expr/*[(AgentDef, NegotiationEnvironmentController#CreateInterface => AgentRef)]*/(
            q"${Raw.TreesBuilder.agents[c.type](c)(rawAgDef, raw)} -> ${agentsCreationExpressions(rawAgDef.name, body)}"
          )
      }

//    c.info(NoPosition, "raw.agents = " + showRaw(raw.agents), true)
//    c.info(NoPosition, "specCompositionsByRaw = " + specCompositionsByRaw.toString(), true)
    c.Expr(
      q"""
        import feh.tec.agents.light.spec.NegotiationSpecification._
        akka.actor.Props(
          new NegotiationEnvironmentController{
            protected lazy val spawns: Map[String, Int] = $spawns
            protected lazy val issues: Map[String, Var] = Map(Seq(..$issues): _*)
            protected lazy val issuesByNegotiation: Map[String, Seq[String]] = Map(Seq(..$issuesByNegotiation): _*)
            protected lazy val initialAgents: List[(AgentDef, CreateInterface => AgentRef)] = List(..$initialAgents)
            protected lazy val systemAgentsInit: Set[() => AgentRef] = Set()
            protected lazy val timeouts = $timeouts
          }
        )
       """
    )
  }

  object TimeoutsDefault extends Timeouts{
    def initialize: Timeout = 100.millis
    def start: Timeout      = 30.millis
    def stop: Timeout       = 30.millis
    def reset: Timeout      = 30.millis
  }


}

class NegotiationControllerBuilder[C <: whitebox.Context](val c: C){
  import c.universe._

  type CreateInterface = c.Expr[(String, NegotiationRole, Set[NegotiationInit])]

  def determineSpecificationComposition(spec: c.Expr[AgentSpecification]) = SpecificationComposition(
    Seq(
      typeCheckOpt[PriorityAndProposalBasedAgentSpec[_, _]](spec.tree, AgentsParts.PriorityAndProposalBased),
      typeCheckOpt[IteratingSpec.AllVars[_, _]](spec.tree, AgentsParts.IteratingAllVars)
    ).flatten
  )

  def dependenciesWithCalls(comp: SpecificationComposition): Map[String, (c.Tree, c.Expr[_])] =
    comp.parts.flatMap(_.interface.descriptions).map{
      case (name, descr) => ???
    }.toMap

  /** @return body@List[Tree] => Expr[Props] */
  def agentPropsExpr(composition: SpecificationComposition, raw: Raw.AgentDef[c.type], dependenciesCalls: Map[String, c.Expr[_]]) = {
    val args = composition.parts.flatMap(_.interface.descriptions.keys)
    val argsSeq = args.map(arg => q"$arg -> ${dependenciesCalls(arg)}")
    val argsMap = q"Map(Seq(..$argsSeq): _*)"

/*
RefinedType(
  List(
    TypeRef(ThisType(scala), TypeName("AnyRef"), List()),
    TypeRef(SingleType(SingleType(ThisType(feh.tec.agents.light.impl.agent), create), PPI), AllVarsSpec, List())
  ),
  Scope()
)
*/

/* tSign:
ClassInfoType(
  List(
    TypeRef(ThisType(scala), TypeName("AnyRef"), List()),
    TypeRef(
      ThisType(feh.tec.agents.light.impl.spec),
      feh.tec.agents.light.impl.spec.PriorityAndProposalBasedAgentSpec,
      List(
        TypeRef(ThisType(feh.tec.agents.light.impl.agent.create.PPI), TypeName("Ag"), List()),
        TypeRef(ThisType(feh.tec.agents.light.impl.agent.create.PPI), TypeName("Lang"), List())
      )
    ),
    TypeRef(
      SingleType(ThisType(feh.tec.agents.light.impl.spec), feh.tec.agents.light.impl.spec.IteratingSpec),
      feh.tec.agents.light.impl.spec.IteratingSpec.AllVars,
      List(
        TypeRef(ThisType(feh.tec.agents.light.impl.agent.create.PPI), TypeName("Ag"), List()),
        TypeRef(ThisType(feh.tec.agents.light.impl.agent.create.PPI), TypeName("Lang"), List())
      )
    ),
    TypeRef(
      ThisType(feh.tec.agents.light.impl.agent.create),
      feh.tec.agents.light.impl.agent.create.SpecExt,
      List(
        TypeRef(ThisType(feh.tec.agents.light.impl.agent.create.PPI), TypeName("Ag"), List())
      )
    )
  ),
  Scope(),
  feh.tec.agents.light.impl.agent.create.PPI.AllVarsSpec
)
*/

    val constructor =
      (interface: CreateInterface) =>
        DefDef(
          Modifiers(),
          termNames.CONSTRUCTOR,
          Nil,
          Nil,
          TypeTree(),
          Block(
            List(
              Apply(
                Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), {
                  def selI(i: Int) = Select(interface.tree, TermName("_" + i))
                  List(selI(1), selI(2), selI(3), argsMap)
                }
              )
            ),
            Literal(Constant(()))
          )
        )
    def template =
      (interface: CreateInterface, body: List[c.Tree]) =>
        Template(
          parents = composition.parts.toList.map(_.parentTree[c.type](c)),
          self = noSelfType,
          body = constructor(interface) :: body
        )
    def classDef =
      (interface: CreateInterface, body: List[c.Tree]) =>
        ClassDef(Modifiers(Flag.FINAL), TypeName("AnonAgentClass"), List(), template(interface, body))

    
    (body: List[c.Tree]) =>
      q"""
        ((interface: CreateInterface) => {
          ${classDef(c.Expr(Ident(TermName("interface"))), body)}
          akka.actor.Props(classOf[AnonAgentClass])
        })
      """
    }

  def buildVar = (_: Raw.VarDef[c.type]) match {
    case Raw.VarDef(name, DomainDef(_domain, _tpe, _dTpe)) =>
      val dTpe = _dTpe.asInstanceOf[c.Type]
      val tpe = _tpe.asInstanceOf[c.Type]
      val domain = _domain.asInstanceOf[c.Tree]

      def buildVar(dom: c.Tree) = q"new Var($name, classOf[$tpe].isInstance) with $dom{ def domain: $dTpe = $domain }"

      val domTraitTree = dTpe match {
        case t if t.asInstanceOf[c.Type] =:= typeOf[Range]     => tq"Domain.Range"
        case t if t.asInstanceOf[c.Type] <:< typeOf[Iterable[_]]  => tq"Domain.Small"
      }
      name -> buildVar(domTraitTree)
  }

  def typesOf(raw: Raw.AgentDef[c.type]) = {

    val tSign = raw.specTpe match {
      case RefinedType(tps, _) => tps.filter(_ <:< typeOf[AgentSpecification]).map(_.typeSymbol.asClass.typeSignature)
    }

    val (agTpe, langTpe) = tSign.flatMap{
      case ClassInfoType(parents, _, _) => parents.filter(_ <:< typeOf[AgentSpecification]).map{
        case TypeRef(pre, sym, tArgs) =>
          val ag    = tArgs.filter(_ <:< typeOf[NegotiatingAgent[_]]).ensuring(_.size == 1).head
          val lang  = tArgs.filter(_ <:< typeOf[NegotiationLanguage]).ensuring(_.size == 1).head
          ag -> lang
      }
    }.unzip

    agTpe.toSet -> langTpe.toSet
  }

  def createNegotiationAndNegotiationTypeTree(agType: Set[c.Type]) = {
    val neg = agType.flatMap(_.decls).withFilter(_.isType).map(_.asType).find(_.name == TypeName("Negotiation")).get


    //protected def createNegotiation(id: NegotiationId): Negotiation

    def idTree(id: c.Expr[NegotiationId]) = ValDef(Modifiers(), TermName("id"), TypeTree(typeOf[NegotiationId]), id.tree)
    def issuesTree(id: c.Expr[NegotiationId]) = ValDef(Modifiers(), TermName("issues"), TypeTree(typeOf[Set[Var]]),
      q"negotiationsInit.find(_.id == $id).get.issues"
    )
    val scopeUpdateTree = DefDef(Modifiers(), TermName("scopeUpdated"), Nil, List(Nil), TypeTree(typeOf[Unit]), q"{}")

    val negParents: List[c.Tree] = neg.typeSignature match {
      case TypeBounds(_, RefinedType(upBounds)) => upBounds match {
        case (l: List[_], _) => l.asInstanceOf[List[c.Type]].map(TypeTree(_))
        case x => c.abort(NoPosition, "!!1!! " + showRaw(x))
      }
      case x => c.abort(NoPosition, "!!2!! " + showRaw(x))
    }

//    c.abort(NoPosition,  "!!3!! " + showRaw(negParents))

    val parents = tq"Negotiation.DynamicScope" :: negParents

    def negotiationTree(id: c.Expr[NegotiationId]): c.Tree ={
      val constructor =
        DefDef(Modifiers(), termNames.CONSTRUCTOR, Nil, List(Nil), TypeTree(),
          Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))
//          Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))
        )
      Block(
        ClassDef(Modifiers(Flag.FINAL), TypeName("$anon"), Nil,
          Template(
            parents = parents,
            self = noSelfType,
            body = constructor :: idTree(id) :: issuesTree(id) :: scopeUpdateTree :: Nil
          )
        ) :: Nil,
        Apply(Select(New(Ident(TypeName("$anon"))), termNames.CONSTRUCTOR), List())
      )
    }

    TypeDef(Modifiers(), TypeName("Negotiation"), Nil, CompoundTypeTree(Template(parents, noSelfType, Nil))) ->
    DefDef(
      Modifiers(Flag.PROTECTED),
      TermName("createNegotiation"),
      Nil,
      List(ValDef(Modifiers(Flag.PARAM), TermName("id"), TypeTree(typeOf[NegotiationId]), EmptyTree) :: Nil),
      TypeTree(Ident(TypeName("Negotiation")).tpe),
      negotiationTree(c.Expr(Ident(TermName("id"))))
    )
  }

/*
Block(
  List(
    ClassDef(
      Modifiers(FINAL),
      TypeName("$anon"),
      List(),
      Template(
        List(
          Select(Select(Select(Select(Select(Select(Select(Ident($line129.$read), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TypeName("C")),
          Select(Select(Select(Select(Select(Select(Select(Ident($line130.$read), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TypeName("D")),
          Select(Select(Select(Select(Select(Select(Select(Ident($line135.$read), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TypeName("E")),
          Select(Select(Select(Select(Select(Select(Select(Ident($line136.$read), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TermName("$iw")), TypeName("F"))
        ),
        noSelfType,
        List(
          DefDef(
            Modifiers(),
            termNames.CONSTRUCTOR,
            List(),
            List(List()),
            TypeTree(),
            Block(
              List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())),
              Literal(Constant(()))
            )
          )
        )
      )
    )
  ),
  Apply(Select(New(Ident(TypeName("$anon"))), termNames.CONSTRUCTOR), List())
)
*/

  protected def typeCheck[T : c.TypeTag](tree: c.Tree) = c.typecheck(tree, c.TERMmode, c.typeOf[T], silent = true).nonEmpty
  protected def typeCheckOpt[T : c.TypeTag](tree: c.Tree, part: => AgentDefinitionPart) = if(typeCheck[T](tree)) Some(part) else None
}

case class SpecificationComposition(parts: Seq[AgentDefinitionPart])

abstract class AgentDefinitionPart(val canBeMixed: Boolean, val interface: AgentCreationInterfaceDescriptor){
//  def parent: Option[AgentDefinitionPart]
  def parentTree[C <: whitebox.Context](c: C): c.Tree
}