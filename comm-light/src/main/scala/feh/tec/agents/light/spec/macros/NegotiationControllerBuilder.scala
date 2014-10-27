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

    val createInterfacesProps = specCompositionsByRaw mapValues (builder.agentPropsExpr(_, dependencyCallsByName)) map {
      case (agRawDef, agentPropsExpr) => agRawDef.name -> agentPropsExpr
    }

    def agentsCreationExpressions(name: String): c.Expr[(String, NegotiationRole, Set[NegotiationInit]) => AgentRef] = c.Expr(
      q"""
        {case (uniqueName, role, negInits) => AgentRef(
          Agent.Id(uniqueName, role),
          implicitly[ActorSystem].actorOf(
            ${createInterfacesProps(name)}(uniqueName, role, negInits))
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
        rawAgDef => c.Expr/*[(AgentDef, NegotiationEnvironmentController#CreateInterface => AgentRef)]*/(
          q"${Raw.TreesBuilder.agents[c.type](c)(rawAgDef, raw)} -> ${agentsCreationExpressions(rawAgDef.name)}"
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

  def agentPropsExpr(composition: SpecificationComposition, dependenciesCalls: Map[String, c.Expr[_]]) = {
    val args = composition.parts.flatMap(_.interface.descriptions.keys)
    val argsSeq = args.map(arg => q"$arg -> ${dependenciesCalls(arg)}")
    val argsMap = q"Map(Seq(..$argsSeq): _*)"

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
      (interface: CreateInterface) =>
        Template(
          parents = composition.parts.toList.map(_.parentTree[c.type](c)),
          self = noSelfType,
          body = constructor(interface) :: Nil
        )
    def classDef =
      (interface: CreateInterface) =>
        ClassDef(Modifiers(Flag.FINAL), TypeName("AnonAgentClass"), List(), template(interface))

/*
    def filterParents(parts: Seq[AgentDefinitionPart]) = Y[List[AgentDefinitionPart], List[c.Tree]](
      rec => {
        case Nil => Nil
        case head :: tail =>
          val h = if(head.parent.exists(tail.contains)) Nil else head.parentTree[c.type](c) :: Nil
          h ::: rec(tail)
      }
    )(parts.toList)
*/

    q"""
      ((interface: CreateInterface) => {
        ${classDef(c.Expr(Ident(TermName("interface"))))}
        akka.actor.Props(classOf[AnonAgentClass])
      })
    """
    //akka.actor.Props(new AnonAgentClass {})(implicitly)

    //      val selProps = Ident(TermName("akka.actor.Props")) // Select(Select(Ident(TermName("akka")), TermName("actor")), TermName("Props"))
//      Block(
//        List(classDef(interface)),
//        Apply(
//          Apply(
//            Select(selProps, TermName("apply")),
//            Apply(Select(New(Ident(TypeName("$anon"))), termNames.CONSTRUCTOR), List()) :: Nil
//          ),
//          List(Select(This(TypeName("Predef")), TermName("implicitly")))
//        )
//      )
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

  protected def typeCheck[T : c.TypeTag](tree: c.Tree) = c.typecheck(tree, c.TERMmode, c.typeOf[T], silent = true).nonEmpty
  protected def typeCheckOpt[T : c.TypeTag](tree: c.Tree, part: => AgentDefinitionPart) = if(typeCheck[T](tree)) Some(part) else None
}

case class SpecificationComposition(parts: Seq[AgentDefinitionPart])

abstract class AgentDefinitionPart(val canBeMixed: Boolean, val interface: AgentCreationInterfaceDescriptor){
//  def parent: Option[AgentDefinitionPart]
  def parentTree[C <: whitebox.Context](c: C): c.Tree
}