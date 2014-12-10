package feh.tec.agents.lite.spec.macros.impl.agent

import akka.actor.ActorRef
import feh.tec.agents.lite
import feh.tec.agents.lite.AgentCreationInterface.NegotiationInit
import feh.tec.agents.lite._
import feh.tec.agents.lite.impl.agent.{ChangingIssuesImpl, FailureChecks}
import feh.tec.agents.lite.impl.spec.{IteratingSpec, PriorityAndProposalBasedAgentSpec}
import feh.tec.agents.lite.impl.agent
import feh.tec.agents.lite.spec.AgentOverride
import feh.tec.agents.lite.spec.macros.AgentsBuildingMacroBase
import scala.concurrent.duration.FiniteDuration
import scala.reflect.macros.whitebox
import feh.util._

/** Contains `MacroSegmentsTransform`s for **Aggregating Parents**
  */
trait AggregateParents[C <: whitebox.Context]{
  self: AgentsBuildingMacroBase[C] =>

  def allAggregatingParents(raw: NegotiationRaw) =
    AgentSegmentParentPriorityAndProposalBased(raw) ::
    AgentSegmentParentIteratingAllVars(raw) ::
    AgentSegmentParentIteratingChangingIssues(raw) ::
    AgentSegmentParentRequiresDistinctPriority(raw) ::
    AgentSegmentParentFailedConfigurationsChecks(raw) ::
    AgentSegmentParentChangingIssues(raw) ::
    AgentSegmentParentFailedPartialSolutionsChecks(raw) ::
    AgentSegmentParentReportingAgent(raw) ::
    AgentSegmentAgentOverride(raw) ::
    AgentSegmentNegotiationSupport(raw) :: Nil

  /** Adds corresponding parent and definitions if `raw`.spec is a [[PriorityAndProposalBasedAgentSpec]]
    */
  def AgentSegmentParentPriorityAndProposalBased(raw: NegotiationRaw) = {
    val priorityAndProposalBased = raw.agents.filter(_.spec.actualType <:< c.typeOf[PriorityAndProposalBasedAgentSpec[_, _]])

    MacroSegmentsTransform{
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment {
          case Trees(controller, ags, extra) =>
            val newAg = ags.map {
              original =>
                priorityAndProposalBased.find(_.name == original._1)
                  .map {
                  case Raw.AgentDef(name, _, _, _) =>
                    name -> original._2
                      .append.parents(
                        c.typeOf[agent.PriorityAndProposalBasedAgent[Language.ProposalBased with Language.HasPriority]]
                      )
                      .add.constructorArgs(
                        "uniqueName" -> c.typeOf[String],
                        "role" -> c.typeOf[NegotiationRole],
                        "negotiationsInit" -> c.typeOf[Set[NegotiationInit]],
                        "args" -> c.typeOf[AgentCreationInterface#Args]
                      )
                }
                  .getOrElse(original)
            }.toMap

            Trees(controller, newAg, extra)
        }
      )}
  }

  /** Adds corresponding parent and definitions if `raw`.spec is a [[IteratingSpec.AllVars]]
    */
  def AgentSegmentParentIteratingAllVars(raw: NegotiationRaw) = {
    val iteratingAllVars = raw.agents.filter(_.spec.actualType <:< c.typeOf[IteratingSpec.AllVars[_, _]])

    MacroSegmentsTransform{
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment {
          case Trees(controller, ags, extra) =>
            val newAgs = ags.map(
              transform(iteratingAllVars) {
                (trees, raw) =>
                  trees
                    .append.parents(
                      c.typeOf[agent.DomainIteratingAllVars[Language.ProposalBased with Language.HasPriority]]
                    )
              }
            )
            Trees(controller, newAgs, extra)
        }
      )
    }
  }

  /** Adds corresponding parent and definitions if `raw`.spec is a [[IteratingSpec.ChangingIssues]]
    */
  def AgentSegmentParentIteratingChangingIssues(raw: NegotiationRaw) = {
    val iteratingChangingIssues = raw.agents.filter(_.spec.actualType <:< c.typeOf[IteratingSpec.ChangingIssues[_, _]])

    MacroSegmentsTransform{
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment {
          case Trees(controller, ags, extra) =>
            val newAgs = ags.map(
              transform(iteratingChangingIssues) {
                (trees, raw) =>
                  trees
                    .append.parents(
                      c.typeOf[agent.DomainIteratingChangingIssues[Language.ProposalBased with Language.HasPriority with Language.NegotiatesIssues]]
                    )
              }
            )
            Trees(controller, newAgs, extra)
        }
      )
    }
  }

  /** Adds corresponding parent and definitions if `raw.spec` is a [[spec.RequiresDistinctPriority]]
    */
  def AgentSegmentParentRequiresDistinctPriority(raw: NegotiationRaw) = {
    import c.universe._

    val requireDistinctPriority = raw.agents.filter(_.spec.actualType <:< c.typeOf[spec.RequiresDistinctPriority])

    val controllerExtra = q"""
        protected object InitialPriority {
          private var p = Map.empty[NegotiationId, Priority]
          def next(neg: NegotiationId) = synchronized{
            val pr = p.getOrElse(neg, new Priority(0)).raise()
            p += neg -> pr
            pr
          }
        }
      """
    def argTree(agent: String) = {
      val entries = raw.agents.find(_.name == agent).get.negotiations.map {
        case Raw.AgentNegDef(negName, _, _, _, _) =>
          val negId = q"NegotiationId($negName)"
          q"$negId -> InitialPriority.next($negId)"
      }
      q"Map(..$entries)"
    }

    MacroSegmentsTransform{
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment {
          case trees@Trees(controller, ags, extra) =>
            val newAgs = ags.map{
              transform(requireDistinctPriority) {
                (trees, raw) =>
                  trees
                    .append.parents(c.typeOf[agent.RequiresDistinctPriority])
                    .append.body(
                      q"""lazy val initialPriority = args("initial-priority").asInstanceOf[Map[NegotiationId, Priority]]"""
                    )
              }
            }
            Trees(if (requireDistinctPriority.nonEmpty) controller.append.body(controllerExtra) else controller, newAgs, extra)
              .addAgentArgs(requireDistinctPriority.map{
                case Raw.AgentDef(name, _, _, _) =>
                  AddAgentArgs(name, "initial-priority", c.typeOf[Map[NegotiationId, Priority]], argTree(name))
              })
        }
      )
    }
  }

  /*protected */def agentType(raw: Raw.AgentDef) = {
    import c.universe._
    raw.spec.actualType.members.find(_.name == TermName("agentTag")).flatMap(
      _.typeSignature.resultType.typeArgs.filter(_ <:< typeOf[AbstractAgent]).ensuring(_.size <= 1).headOption
    )
  }

  /** Adds corresponding parent and definitions if `raw` indicates agent's type is [[FailedConfigurationsChecks]]
    */
  def AgentSegmentParentFailedConfigurationsChecks(raw: NegotiationRaw) = {
    import c.universe._

    val failedConfigurationsChecks = raw.agents.filter(r => agentType(r).exists(_ <:< typeOf[FailedConfigurationsChecks[_]]))
    val failedConfigurationsChecksTpe = typeOf[FailureChecks[Language.ProposalBased with Language.HasPriority]]

    MacroSegmentsTransform {
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment{
          case trees =>
            val newAgs = trees.agents.map{
              transform(failedConfigurationsChecks){
                (tr, raw) =>
                  tr.append.parents(failedConfigurationsChecksTpe)
              }
            }
            trees.copy(agents = newAgs)
        }
      )
    }
  }

  /** Adds corresponding parent and definitions if `raw` indicates agent's type is [[ChangingIssues]]
    */
  def AgentSegmentParentChangingIssues(raw: NegotiationRaw) = {
    import c.universe._

    val changingIssues = raw.agents.filter(r => agentType(r).exists(_ <:< typeOf[ChangingIssues[_]]))
    val changingIssuesTpe = typeOf[ChangingIssuesImpl[Language.ProposalBased with Language.HasPriority with Language.NegotiatesIssues]]

    MacroSegmentsTransform {
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment{
          case trees =>
            val newAgs = trees.agents.map{
              transform(changingIssues){
                (tr, raw) =>
                  tr.append.parents(changingIssuesTpe)
              }
            }
            trees.copy(agents = newAgs)
        }
      )
    }

  }

  /** Adds corresponding parent and definitions if `raw` indicates agent's type is [[FailedPartialSolutionsChecks]]
    */
  def AgentSegmentParentFailedPartialSolutionsChecks(raw: NegotiationRaw) = {
    import c.universe._

    val failedPartialSolutionsChecks = raw.agents.filter(r => agentType(r).exists(_ <:< typeOf[FailedPartialSolutionsChecks[_]]))
    val failedPartialSolutionsChecksTpe =
      typeOf[impl.agent.PartialSolutionsChecks[Language.ProposalBased with Language.HasPriority with Language.NegotiatesIssues]]

    MacroSegmentsTransform {
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment{
          case trees =>
            val newAgs = trees.agents.map{
              transform(failedPartialSolutionsChecks){
                (tr, raw) =>
                  tr.append.parents(failedPartialSolutionsChecksTpe)
              }
            }
            trees.copy(agents = newAgs)
        }
      )
    }
  }


  /** Adds corresponding parent and definitions if any of `raw.negotiations` have `reportingToOpt` defined
    */
  def AgentSegmentParentReportingAgent(raw: NegotiationRaw) = {
    import c.universe._

    val reportingAgents = raw.agents.filter(_.negotiations.exists(_.reportingToOpt.isDefined))

    def agentParent = typeOf[AutoReporting[NegotiationLanguage]]

    def reportListenerRoleTree = q"feh.tec.agents.lite.impl.service.DefaultReportWriter.Role"
    def reportToTree = q"""
        systemAgents.find(_.id.role == $reportListenerRoleTree)
          .getOrElse(sys.error("no system agent for with " + $reportListenerRoleTree + " found"))
      """

    // todo
    def reportSysAgentBody: List[c.Tree] = reportingAgents.map(_.negotiations.map(_.reportingToOpt.get).distinct).flatMap(
      _.map{
        tree =>
          q"for(f <- $tree.forward) self ! feh.tec.agents.lite.AgentReport.Forward(f)"
      }
    ).toList

    def agentConstructorArgs = Map(
      "writeTo" -> typeOf[java.io.File],
      "controller" -> typeOf[ActorRef],
      "confirmAllWaitingDelay" -> typeOf[FiniteDuration]
    )

    def reportSysAgent = "report-listener" -> ActorTrees("$ReportListener",
      parents = typeOf[lite.impl.service.DefaultReportWriter] ::  Nil, //typeOf[NegotiationFinishedListener] :: Nil, // todo: use ReportListenerRef
      body = reportSysAgentBody,
      constructorArgs = agentConstructorArgs
    )

    MacroSegmentsTransform{
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment{
          case Trees(controller, ags, extra) =>
            val newAgs = ags map {
              case (name, trees) if reportingAgents.exists(_.name == name) =>
                val reportingTo = reportingAgents.find(_.name == name).get.negotiations.map{
                  case Raw.AgentNegDef(neg, _, _, Some(_), _) => q"""NegotiationId($neg) -> args("report-to").asInstanceOf[AgentRef]"""
                }

                val tr = trees
                  .append.parents(agentParent)
                  .append.body(
                    q"val reportingTo: Map[NegotiationId, AgentRef] = Map(..$reportingTo)",
                    q"def extraMessage: Map[String, Any] = Map()",
                    q"""def stateReport(negId: NegotiationId) =
                          feh.tec.agents.lite.StateReport(negId, get(negId).report(), "by demand")"""
                  )
                name -> tr
              case p => p
            }

            val sys = if(reportingAgents.nonEmpty) reportSysAgent :: Nil else Nil
            val newController = controller.append.parents(typeOf[lite.impl.service.ReportPrinterSupportBundle])

            Trees(newController, newAgs ++ sys, extra)
              .addAgentArgs(
                reportingAgents.map(aDef => AddAgentArgs(aDef.name, "report-to", typeOf[AgentRef], reportToTree)),
                Seq(
                  AddAgentArgs("report-listener", "writeTo", typeOf[java.io.File], Ident(TermName("reportFile")) ),
                  AddAgentArgs("report-listener", "role", typeOf[Role], reportListenerRoleTree),
                  AddAgentArgs("report-listener", "controller", typeOf[ActorRef], q"self"),
                  AddAgentArgs("report-listener", "confirmAllWaitingDelay", typeOf[FiniteDuration], q"timeouts.`confirm finished`")
                )
              )
        }
      )
    }
  }

  /** Adds parents marked with [[feh.tec.agents.lite.spec.AgentOverride]]
    */
  def AgentSegmentAgentOverride(raw: NegotiationRaw) = {
    import c.universe._

    def recSearch(tpe: Type): List[Type] ={
      val overrideTpes = tpe.baseClasses.map(_.typeSignature).filter(_ <:< typeOf[AgentOverride]).flatMap{
          case RefinedType(tpes, _) => tpes.filter(_ <:< typeOf[AgentOverride])
          case ref: TypeRef => ref :: Nil
        }

      val (impls, toGo) = overrideTpes
        .zipMap(_.baseClasses.map(_.typeSignature).filter(_ <:< typeOf[AgentOverride]))
        .partition(_._2.isEmpty)

      impls.unzip._1.toList ::: toGo.flatMap(_._2).distinct.flatMap(recSearch).toList

    }

    val markedOverride = raw.agents.flatMap{
      ag =>
        agentType(ag)
              .map(t => ag -> recSearch(t))
    }.toMap

    MacroSegmentsTransform{
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment{
          case tr@Trees(_, ags, _) =>
            val newAgs = ags.map(transform(markedOverride.keys.toList){
              (tr, raw) =>
                tr.append.parents(markedOverride(raw): _*)
            })

            tr.copy(agents = newAgs)
        }
      )
    }
  }

  /** Adds parents marked with [[NegotiationSupport]]
    */
  def AgentSegmentNegotiationSupport(raw: NegotiationRaw) = {
    import c.universe._

    val negotiationSupport = raw.agents.zipMap(agentType)
      .withFilter(_._2.isDefined).map(p => p._1 -> p._2.get)
      .filter(_._2 <:< typeOf[NegotiationSupport])
      .map(_._1).toList

    MacroSegmentsTransform(
      _.append(AgentBuildingStages.AggregateParents,
        MacroSegment{
          case tr@Trees(_, agents, _) =>
            val newAgs = agents.map(transform(negotiationSupport){
              (tr, _) => tr
                .append.parents(typeOf[impl.agent.NegotiationNotifications])
                .append.body(q"""protected def controllerRef = args("controller-ref").asInstanceOf[AgentRef]""")
            })

            tr
              .copy(agents = newAgs)
              .addAgentArgs(negotiationSupport.map{
                raw =>
                  AddAgentArgs(raw.name, "controller-ref", typeOf[AgentRef], q"this.ref") //q"implicitly[AgentRef]"
              })
        }
      )
    )
  }
}
