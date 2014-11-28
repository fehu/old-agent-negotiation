package feh.tec.agents.lite.spec.macros.exp.impl.agent

import feh.tec.agents.lite.spec.NegotiationSpecification
import feh.tec.agents.lite._
import feh.util._
import feh.tec.agents.lite.spec.macros.exp.AgentsBuildingMacroExperimentalBase
import scala.concurrent.duration.FiniteDuration
import scala.reflect.macros.whitebox

/** Contains `MacroSegmentsTransform`s for `ValAndDefDefinitions` stage
 */
trait ValAndDefDefinitions [C <: whitebox.Context]{
  self: AgentsBuildingMacroExperimentalBase[C] with TypesDefinitionsHelpers[C] =>

  def allValAndDefDefinitions(raw: NegotiationRaw) =
    AgentSegmentCreateNegotiation ::
    AgentSegmentDomainIterators ::
    AgentSegmentConstraintsByNegotiation ::
    AgentSegmentSpec(raw) ::
    AgentSegmentResponseDelay :: Nil

  import c.universe._

  /** creates `createNegotiation` method
   */
  def AgentSegmentCreateNegotiation = {

    def negotiationScopeUpdatedTree = q"{}"

    def createNegotiationTreeOpt(ag: ActorTrees) = {
      val implTpesOpt = ag.body
        .collect{
        case TypeDef(_, TypeName("Negotiation"), Nil, tree) =>
          tree match {
            case CompoundTypeTree(Template(tpes, _, _)) => tpes
          }
      }
        .ensuring(_.size <= 1).headOption

      val negotiationCreation = ag.parents.exists(_ <:< typeOf[impl.agent.NegotiationCreation])

      implTpesOpt map {
        impl =>
          q"""
            protected def createNegotiation(nId: NegotiationId): Negotiation = {
              val neg = new ${impl.head} with ..${impl.tail} {
                val id = nId
                val issues: Set[Var] = negotiationsInit.find(_.id == id).get.issues
                def scopeUpdated(): Unit = $negotiationScopeUpdatedTree
              }
              ${if(negotiationCreation) q"negotiationCreated(neg)" else q"" }
              neg
            }
          """
      }
    }

    MacroSegmentsTransform(
      _.append(AgentBuildingStages.ValAndDefDefinitions,
        MacroSegment{
          case trees =>
            val newAgs = trees.agents map {
              case (name, agTr) =>
                val opt = createNegotiationTreeOpt(agTr)
                name -> agTr.append.body(opt.toSeq: _*)
            }
            trees.copy(agents = newAgs)
        }
      )
    )
  }

  /** creates `domainIterators` method
    */
  def AgentSegmentDomainIterators = {
    def domainIteratorsArg = "domain-iterators"
    def domainIteratorsType = typeOf[Map[Var, DomainIteratorBuilder[Any, Any]]]
    def getDomainIterators = q"issues.values.map(v => v -> domainIteratorsCreators(v.name)).toMap"
    def addDomainIteratorsArg(agName: String) = AddAgentArgs(agName, domainIteratorsArg, domainIteratorsType, getDomainIterators)
    def domainIteratorsTree = q"""
      def domainIterators = args($domainIteratorsArg).asInstanceOf[Map[Var, DomainIteratorBuilder[Var#Domain, Var#Tpe]]]
    """

    MacroSegmentsTransform(_
      .append(AgentBuildingStages.ValAndDefDefinitions,
        MacroSegment{
          case trees =>
            val newAgs = transformIfHasMember(trees.agents)(
              isAbstract(TermName("domainIterators")), { case (name, tr) => tr.append.body(domainIteratorsTree) }
            )
            trees
              .copy(agents = newAgs)
              .addAgentArgs(newAgs.keySet.toList.map(addDomainIteratorsArg))
        }

      )
    )
  }

  def AgentSegmentConstraintsByNegotiation = {

    def constraintsByNegotiationArg = "constraints-by-negotiation"
    def constraintsByNegotiationType = typeOf[Map[NegotiationId, NegotiationSpecification.AgentConstraintsDef]]
    def getConstraintsByNegotiation(agName: String) = q"""
      import feh.tec.agents.lite.spec.NegotiationSpecification

      initialAgents.find(_._1.name == $agName).get._1.negotiations.map{
        case NegotiationSpecification.AgentNegDef(negName, _, extra) =>
          val constraints = extra.collect{
            case c: NegotiationSpecification.AgentConstraintsDef => c
          }
          NegotiationId(negName) -> constraints.ensuring(_.size == 1, constraints.size + " AgentConstraintsDef defined").head
      }.toMap
    """

    def addConstraintsByNegotiationArg(agName: String) =
      AddAgentArgs(agName, constraintsByNegotiationArg, constraintsByNegotiationType, getConstraintsByNegotiation(agName))


    def seqToTuple = q"""
      (s: Seq[Any]) => ${
      Match(q"s", (for(i <- 2 to 22) yield
        cq"seq if seq.length == $i => ${TermName("Tuple"+i)}(..${for (j <- 0 until i) yield q"seq($j)"})".asInstanceOf[CaseDef]).toList)
    }"""

    def constraintsByNegotiationTree = q"""
      val constraintsByNegotiation = args($constraintsByNegotiationArg).asInstanceOf[$constraintsByNegotiationType]
      protected def seqToTuple(seq: Seq[Any]): Product = $seqToTuple(seq)
      """

    MacroSegmentsTransform(
      _.append(AgentBuildingStages.ValAndDefDefinitions,
        MacroSegment{
          case trees =>
            val newAgs = transformIfHasMember(trees.agents)(
            isAbstract(TermName("constraintsByNegotiation")), { case (name, tr) => tr.append.body(constraintsByNegotiationTree) }
            )
            trees
              .copy(agents = newAgs)
              .addAgentArgs(newAgs.keySet.toList.map(addConstraintsByNegotiationArg))
        }

      )
    )
  }

  def AgentSegmentSpec(raw: NegotiationRaw) = {
    val specByAgentName = raw.agents.map(ag => ag.name -> ag.spec).toMap

    MacroSegmentsTransform(
      _.append(AgentBuildingStages.ValAndDefDefinitions,
        MacroSegment{
          case trees =>
            val newAgs = trees.agents.map{
              case (name, tr) if specByAgentName contains name =>
                val agentType = tr.body.collect{
                  case TypeDef(_, TypeName("Agent"), Nil, CompoundTypeTree(Template(parents, _, _))) =>
                    internal.refinedType(parents.map(_.tpe), internal.newScopeWith())
                }.head
                val specTpe = tr.parents
                  .flatMap(_.members.find(isAbstract(TermName("spec"))))
                  .map(_.typeSignature.resultType).sortWith(_ <:< _)
                  .last
                  .pipe(replaceLang(tr))
                  .pipe(replaceTypeArg(_, typeOf[AbstractAgent], agentType))


                val sepcDef = q"val spec = ${specByAgentName(name)}.asInstanceOf[$specTpe]"
                name -> tr.append.body(sepcDef)
              case other => other
            }
            trees.copy(agents = newAgs)
        }
      )
    )
  }


  def AgentSegmentResponseDelay = {

    def responseDelayArgName = "response-delay"
    def responseDelayArgType = typeOf[FiniteDuration]
    def responseDelayAgBody = q"""protected lazy val responseDelay = args($responseDelayArgName).asInstanceOf[$responseDelayArgType]"""
    def responseDelayControllerBody = q"timeouts.`response delay`"
    def addResponseDelayArg(agName: String) = AddAgentArgs(agName, responseDelayArgName, responseDelayArgType, responseDelayControllerBody)

    MacroSegmentsTransform(
      _.append(AgentBuildingStages.ValAndDefDefinitions,
        MacroSegment{
          case trees =>
            val newAgs = trees.agents.map{
              case (name, tr@ActorTrees(_, parents, _, _)) if parents.exists(_ <:< typeOf[ResponseDelay[Language]].erasure) =>
                name -> tr.append.body(responseDelayAgBody)
              case other => other
            }

            trees
              .copy(agents = newAgs)
              .addAgentArgs(newAgs.keySet.toList.map(addResponseDelayArg))
        }
      )
    )
  }


  protected def transformIfHasMember(ags: Map[String, ActorTrees])(cond: Symbol => Boolean, f: ((String, ActorTrees)) => ActorTrees) =
    ags.map{
      case (name, ag) =>
        name -> ( if(ag.parents.exists(_.members.exists(cond))) f(name -> ag) else ag )
    }

  protected def isAbstract(name: Name): Symbol => Boolean = sym => sym.isAbstract && sym.name == name
}
