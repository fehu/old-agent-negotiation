package feh.tec.agents.light.spec.macros

import akka.actor.Props
import akka.util.Timeout
import feh.tec.agents.light.AgentCreationInterface.NegotiationInit
import feh.tec.agents.light.impl.NegotiationEnvironmentController
import feh.tec.agents.light.impl.NegotiationEnvironmentController.Timeouts
import feh.tec.agents.light.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.light.spec.AgentSpecification
import feh.tec.agents.light.spec.NegotiationSpecification.{AgentDef, NegotiationDef}
import feh.tec.agents.light.spec.macros.NegotiationSpecificationBuilder.{VarsSeparatingConstrainsBuilder, SimpleConstrainsBuilder, Raw}
import feh.tec.agents.light._
import feh.tec.agents.light.spec.macros.NegotiationSpecificationBuilder.Raw.{DomainDef, VarDef, AgentNegDef}
import feh.util._
import scala.reflect.api.Scopes
import scala.reflect.macros.whitebox
import scala.concurrent.duration._

object NegotiationControllerBuilder {
  def build(c: whitebox.Context)(dsl: c.Expr[spec.dsl.Negotiation]): c.Expr[Props] = {
    import c.universe._

    val builder = new NegotiationControllerBuilder[c.type](c)
    val raw = NegotiationSpecificationBuilder.raw[c.type](c)(dsl)
    implicit val cb = new VarsSeparatingConstrainsBuilder
    
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

    val extraAgentParents = tq"feh.tec.agents.light.impl.agent.DomainIteratorsDefault" :: Nil

    case class ControllerExtra(name: String, parents: List[c.Tree], body: List[c.Tree]){
      override def equals(obj: scala.Any): Boolean = PartialFunction.cond(obj){ case ce: ControllerExtra => ce.name == this.name }
      override def hashCode(): Int = name.hashCode
    }

    val createInterfacesPropsByBody = specCompositionsByRaw map(p => p._1 -> builder.agentPropsExpr(p._2, p._1, dependencyCallsByName, extraAgentParents)) map {
      case (agRawDef, (reportTo, agentPropsExpr)) =>
        val moreControllerParents = tq"feh.tec.agents.light.impl.service.ReportPrinterSupportBundle" :: Nil
        val moreControllerBody =
          q"def configInfo = feh.tec.agents.light.impl.service.SupportBundle.Config(${Configs.controller[c.type](c)})" :: Nil

        agRawDef.name -> (agentPropsExpr -> Some(ControllerExtra("reporting", moreControllerParents, moreControllerBody)))
      case (agRawDef, (Nil, agentPropsExpr)) => agRawDef.name -> (agentPropsExpr -> Option.empty[ControllerExtra])
    }

    def agentsCreationExpressions(name: String, body: List[c.Tree]): (c.Expr[(String, NegotiationRole, Set[NegotiationInit]) => AgentRef], Option[ControllerExtra]) = {
      val (ff, extrasOpt) = createInterfacesPropsByBody(name)
      val f = ff(body)
      c.info(NoPosition, "agentsCreationExpressions:f = " + showCode(f), true)

      c.Expr(
        q"""
        {case (uniqueName, role, negInits) =>
          val props = $f(uniqueName, role, negInits)
          AgentRef(
            Agent.Id(uniqueName, role),
            {
              val acRef = implicitly[ActorSystem].actorOf(props , uniqueName)
              log.debug("actor just created (agentsCreationExpressions) props=" + props + ", acRef= " + acRef)
              acRef
            }
          )
        }
      """
      ) -> extrasOpt
    }

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
    val initialAgentsAndExtra: (List[c.Expr[(AgentDef, NegotiationEnvironmentController#CreateInterface => AgentRef)]], List[Option[ControllerExtra]]) =
      specCompositionsByRaw.toList.map{
        case (rawAgDef, SpecificationComposition(parts)) =>
          val (agTpe, _langTpe) = parts.map(builder.typesOf).unzip
          val langTpes = _langTpe.flatten.distinct

          val langTpe = internal.refinedType(langTpes.toList, internal.newScopeWith())
//          val (negotiationTypeTree, createNegotiationTree) = builder.createNegotiationAndNegotiationTypeTree(agTpe.toSet, langTpe, )
//
//          c.info(NoPosition,
//            "negotiationTypeTree: " + showCode(negotiationTypeTree) +
//            "\ncreateNegotiationTree: " + showCode(createNegotiationTree)
//            , true)
//
//          val body = negotiationTypeTree :: createNegotiationTree :: Nil

          val (agExpr, extraOpt) = agentsCreationExpressions(rawAgDef.name, Nil)
          c.Expr(
            q"${Raw.TreesBuilder.agents[c.type](c)(rawAgDef, raw)} -> $agExpr"
          ) -> extraOpt
      }.unzip

    val initialAgents = initialAgentsAndExtra._1
    val controllerExtras = initialAgentsAndExtra._2.flatten.toList.distinct

    val extraControllerParents = controllerExtras.flatMap(_.parents)
    val extraControllerBody    = controllerExtras.flatMap(_.body)

    c.Expr(
      q"""
        import feh.tec.agents.light.spec.NegotiationSpecification._
        import feh.tec.agents.light.impl.NegotiationEnvironmentController
        akka.actor.Props(
          new NegotiationEnvironmentController with ..$extraControllerParents {
            protected lazy val spawns: Map[String, Int] = $spawns
            protected lazy val issues: Map[String, Var] = Map(Seq(..$issues): _*)
            protected lazy val issuesByNegotiation: Map[String, Seq[String]] = Map(Seq(..$issuesByNegotiation): _*)
            protected lazy val initialAgents: List[(AgentDef, CreateInterface => AgentRef)] = List(..$initialAgents)
            protected lazy val systemAgentsInit: Set[() => AgentRef] = Set()
            protected lazy val timeouts = $timeouts

            ..$extraControllerBody
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

  /** @return reports@Seq(neg name, Tree:ReportListenerRef) -> body@List[Tree] => Expr[Props] */
  def agentPropsExpr(composition: SpecificationComposition,
                     raw: Raw.AgentDef[c.type],
                     dependenciesCalls: Map[String, c.Expr[_]],
                     extraParents: List[c.Tree]) = {
    val descriptionsDefinedArgs = composition.parts.flatMap(_.interface.descriptions.keys)
    val argsDependancies = descriptionsDefinedArgs.map(arg => arg -> dependenciesCalls(arg).tree).toMap

    def argsMap(m: Map[String, c.Tree]) = {
      val entries = m.map{case (k, v) => q"$k -> $v"}
      q"Map(Seq(..$entries): _*)"
    }

    def constructor(args: Map[String, c.Tree]) =
      (interface: CreateInterface) =>
        DefDef(
          Modifiers(),
          termNames.CONSTRUCTOR,
          List(),
          List(List()),
          TypeTree(),
          Block(
            List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR),
            {
              def selI(i: Int) = Select(interface.tree, TermName("_" + i))
              List(selI(1), selI(2), selI(3), argsMap(args))
            })),
            Literal(Constant(()))
          )
        )

//    def specDefTree = //ValDef(Modifiers(Flag.LAZY), TermName("spec"), TypeTree(),
//      q"""
//          implicit class ToResultWrapper(a: Any){ def anyResult[R]: R = a.asInstanceOf[R] }
//          lazy val spec = ${raw.spec.tree.asInstanceOf[c.Tree]}.anyResult
//      """
//    )

    val (agTpe, _langTpe) = composition.parts.map(typesOf).unzip
    val langTpe = langsTpe(_langTpe.flatten)

    val agentType = internal.refinedType(agTpe.toList, internal.newScopeWith())
    //CompoundTypeTree(Template(agTpe.toList.map(TypeTree(_)), noSelfType, Nil))

    val specSymbols = agTpe.flatMap(_.decls.filter(_.typeSignature.resultType <:< typeOf[AgentSpecification]))
    val specSignatures = specSymbols.map(_.typeSignature)

    def specResTpes = specSignatures.flatMap{
      case NullaryMethodType(ref: TypeRef) => replaceSpecSignature(ref) :: Nil
      case NullaryMethodType(RefinedType(tpes, _)) => tpes.map{
        case ref: TypeRef => replaceSpecSignature(ref)
        case other => c.abort(NoPosition, "#specResTpe: " + showRaw(other))
      }
      case other => c.abort(NoPosition, "#specResTpe2: " + showRaw(other))
    }

//    c.abort(NoPosition, "agentType.tpe = " + agentType)

    def replaceSpecSignature(in: TypeRef) = in match {
      case TypeRef(pre, sym, targs) =>
        val newArgs = targs map {
          case TypeRef(_, sm, _) if sm.name == TypeName("Lang") => langTpe
          case TypeRef(ThisType(thisSm), sm, _) if sm.name == TypeName("Agent") => agentType
          case other => c.abort(NoPosition, "#replaceSpecSignature2: " + showRaw(other)) //other
        }
        internal.typeRef(pre, sym, newArgs)
      case other => c.abort(NoPosition, "#replaceSpecSignature3: " + showRaw(other))
    }

    def specResTpe = CompoundTypeTree(Template(specResTpes.toList.map(TypeTree(_)), noSelfType, Nil))

    def agentTypeDef = q"type Agent = $agentType"
    def specDef = q"lazy val spec: $specResTpe = ${raw.spec.tree.asInstanceOf[c.Tree]}.asInstanceOf[$specResTpe]"

    def logOnCreate = q"""log.debug("I've been created")"""

    val reports = raw.negotiations.collect{
      case Raw.AgentNegDef(neg, _, _, Some(reportTo), _) => neg -> reportTo
    }

    def reports_? = reports.nonEmpty

    val (reportsParents, reportsBody, reportsArgs) =  if(reports_?){
      (
        List(tq"AutoReporting[$langTpe]"),
        List(
          q"""
            lazy val reportingTo = {
              log.debug("args = " + args)
              args("reportingTo").asInstanceOf[Map[NegotiationId, AgentRef]]
            }
          """
        ),
        List({
          val reportToEntries = reports.map{ case (negName, reportListenerRefTree) => q"$negName -> reportListener($reportListenerRefTree)" }
          "reportTo" -> q"Map(Seq(..$reportToEntries):_*)"
        })
      )
    } else (Nil, Nil, Nil)

    val extraNegotiationParents = if(reports_?) tq"Negotiation.ChangeHooks" :: Nil else Nil
    val (negotiationTypeTree, createNegotiationTree) = createNegotiationAndNegotiationTypeTree(agTpe.toSet, langTpe, extraNegotiationParents)

    c.info(NoPosition,
      "negotiationTypeTree: " + showCode(negotiationTypeTree) +
      "\ncreateNegotiationTree: " + showCode(createNegotiationTree) +
      "\nextraNegotiationParents: " + extraNegotiationParents.map(showCode(_)).mkString("\n")
      , true)


    val moreParents = reportsParents ::: Nil
    val moreBody = negotiationTypeTree :: createNegotiationTree :: reportsBody

    val args = argsDependancies ++ reportsArgs

    def template =
      (interface: CreateInterface, body: List[c.Tree]) =>
        Template(
          parents = composition.parts.toList.map(_.parentTree[c.type](c)) ::: moreParents ::: extraParents,
//          parents = AgentsParts.IteratingAllVars.parentTree[c.type](c) :: extraParents,// composition.parts.toList.map(_.parentTree[c.type](c)) ::: extraParents,
          self = noSelfType,
          body = constructor(args)(interface) :: logOnCreate :: agentTypeDef :: specDef :: moreBody ::: body
        )
    def classDef =
      (interface: CreateInterface, body: List[c.Tree]) =>
        ClassDef(Modifiers(Flag.FINAL), TypeName("AnonAgentClass"), List(), template(interface, body))


    reports ->
      ((body: List[c.Tree]) =>
        q"""
          ((interface: CreateInterface) => {
            ${classDef(c.Expr(Ident(TermName("interface"))), body)}
            val props = akka.actor.Props(new AnonAgentClass)
            log.debug("Agent class defined " + classOf[AnonAgentClass] + "; it's props =  " + props)
            props
          })
        """
      )
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

  def typesOf(part: AgentDefinitionPart) = {



        //    {
        //      case RefinedType(tps, _) => tps.filter(_ <:< typeOf[AgentSpecification]).map(_.typeSymbol.asClass.typeSignature)
        //      case unkn => c.abort(c.enclosingPosition, showRaw(unkn))
        //    }

    val agTpe = part.tpe[c.type](c)

    val langTpe = part.tpe[c.type](c) match {
      case TypeRef(_, _, List(langType)) =>
        langType match{
          case RefinedType(parents, _) => parents
          case _ => c.abort(NoPosition, s"#langTpe: ${showRaw(langType)}")
        }
      }
//      val (agTpe, langTpe) = tpe match {
//      case ClassInfoType(parents, _, _) => parents.filter(_ <:< typeOf[AgentSpecification]).map{
//        case TypeRef(pre, sym, tArgs) =>
//          val ag    = tArgs.filter(_ <:< typeOf[NegotiatingAgent[_]]).ensuring(_.size == 1).head
//          val lang  = tArgs.filter(_ <:< typeOf[NegotiationLanguage]).ensuring(_.size == 1).head
//          ag -> lang
//      }
//    }.unzip

    agTpe -> langTpe
  }

  def createNegotiationAndNegotiationTypeTree(agType: Set[c.Type], langTpe: c.Type, extraParents: List[c.Tree]) = {
    val negs = agType
      .flatMap(_.decls).withFilter(_.isType).map(_.asType).filter(_.name == TypeName("Negotiation"))
      .ensuring(_.nonEmpty, "#createNegotiationAndNegotiationTypeTree")


    //protected def createNegotiation(id: NegotiationId): Negotiation

    def idTree(id: c.Expr[NegotiationId]) = ValDef(Modifiers(), TermName("id"), TypeTree(typeOf[NegotiationId]), id.tree)
    def issuesTree(id: c.Expr[NegotiationId]) = ValDef(Modifiers(), TermName("issues"), TypeTree(typeOf[Set[Var]]),
      q"""
         import feh.util._
         negotiationsInit
           .find(_.id == $id)
           .getOrThrow("#issues //issuesTree:\n\tnegotiationsInit=" + negotiationsInit + "\n\tid=" + $id)
           .issues
       """
    )
    val scopeUpdateTree = DefDef(Modifiers(), TermName("scopeUpdated"), Nil, List(Nil), TypeTree(typeOf[Unit]), q"{}")

    val negParents: List[c.Tree] = negs.map(_.typeSignature).flatMap{
      case TypeBounds(_, RefinedType(upBounds)) => upBounds match {
        case (l: List[_], _) => l.asInstanceOf[List[c.Type]].map(TypeTree(_))
        case x => c.abort(NoPosition, "!!1!! " + showRaw(x))
      }
      case x => c.abort(NoPosition, "!!2!! " + showRaw(x))
    }.toList

//    c.abort(NoPosition,  "!!3!! " + showRaw(negParents))

//    c.abort(NoPosition, "negParents: " + showRaw(negParents.map(_.tpe.typeArgs)))

    def distinctNegParents = {
      var contains = List.empty[c.Tree]
      for{
        p <- negParents
        if !contains.exists(_.symbol.name == p.symbol.name)
      } contains +:= p
      contains
    }

    val parentsByNegType = distinctNegParents map {
      case tt@TypeTree() if tt.tpe.typeArgs.nonEmpty =>
        val ttpe = tt.tpe match {
          case TypeRef(pre, sym, args) =>
            val newArgs = args map{
              case tpe if tpe <:< typeOf[NegotiationLanguage] => langTpe
              case tpe => tpe
            }
            internal.typeRef(pre, sym, newArgs)
        }
        TypeTree(ttpe)
//        CompoundTypeTree(Template(q.map(TypeTree(_)), noSelfType, Nil))
//        c.abort(NoPosition, "^^^ : " + showRaw(tt) + "\n" + showRaw(tt.tpe) + "\nq: " + showRaw(q) + "\nlangTpe: " + langTpe)
      case p => p
    }

    val parents = parentsByNegType ::: extraParents

    c.info(NoPosition, s"negParents = $negParents\nparents = $parents", true)

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
      List(ValDef(Modifiers(Flag.PARAM), TermName("negId"), TypeTree(typeOf[NegotiationId]), EmptyTree) :: Nil),
      TypeTree(Ident(TypeName("Negotiation")).tpe),
      negotiationTree(c.Expr(Ident(TermName("negId"))))
    )
  }

  def langsTpe(_langTpe: Seq[c.Type]) = internal.refinedType(_langTpe.distinct.toList, internal.newScopeWith())

  def defLangType(langTpe: c.Type) = q"type Lang = $langTpe"

  protected def typeCheck[T : c.TypeTag](tree: c.Tree) = c.typecheck(tree, c.TERMmode, c.typeOf[T], silent = true).nonEmpty
  protected def typeCheckOpt[T : c.TypeTag](tree: c.Tree, part: => AgentDefinitionPart) = if(typeCheck[T](tree)) Some(part) else None
}

case class SpecificationComposition(parts: Seq[AgentDefinitionPart])

abstract class AgentDefinitionPart(val canBeMixed: Boolean, val interface: AgentCreationInterfaceDescriptor){
//  def langTpe[C <: whitebox.Context](c: C)
  def tpe[C <: whitebox.Context](c: C): c.Type
  def parentTree[C <: whitebox.Context](c: C): c.Tree
}