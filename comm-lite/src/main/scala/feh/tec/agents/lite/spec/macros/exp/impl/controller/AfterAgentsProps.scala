package feh.tec.agents.lite.spec.macros.exp.impl.controller

import feh.tec.agents.lite.impl
import feh.tec.agents.lite.spec.macros.Configs
import feh.tec.agents.lite.spec.macros.exp.{AgentsBuildingMacroExperimentalBase, ControllerBuildingMacroExperimentalBase}

import scala.concurrent.duration.FiniteDuration
import scala.reflect.macros.whitebox

/** Contains `MacroSegmentsTransform`s for stages after **EmbedAgentProps**
  */
trait AfterAgentsProps[C <: whitebox.Context]{
  self: ControllerBuildingMacroExperimentalBase[C] with AgentsBuildingMacroExperimentalBase[C] =>

  def allAfterAgentsProps(raw: NegotiationRaw) =
    ControllerSegmentExtraArgsValues ::
    ControllerSegmentEmbedSpawnsAndTimeouts(raw) ::
    ControllerSegmentEmbedControllerDefs(raw) ::
    ControllerSegmentSupportBundle :: Nil

  def ControllerSegmentExtraArgsValues = MacroSegmentsTransform(
    seg => seg.append(ControllerBuildingStages.EmbedExtraArgsValues,
      MacroSegment{
        case trees@Trees(controller, ags) =>
          import c.universe._

          val liftedArgsByNameAndAg = ags map { case (agName, _) => agName -> q"${
            agentArgsRequired(seg)(agName).mapValues(p => q"() => ${p._2}")}" }

          val extraArgs = q"""
            private lazy val liftedArgsByNameAndAg: Map[String, Map[String, () => Any]] =
              Map(..${
                liftedArgsByNameAndAg.map{ case (name, tree) => q"$name -> $tree" }
              })
            protected def extraArgs(agent: String): Map[String, Any] = liftedArgsByNameAndAg(agent).map{
              case (n, f) =>
                n -> f()
            }
          """

          trees.copy(controller = controller.append.body(extraArgs))
      }

    )
  )

  def ControllerSegmentEmbedSpawnsAndTimeouts(raw: NegotiationRaw) = MacroSegmentsTransform(
    _.append(ControllerBuildingStages.EmbedSpawnsAndTimeouts,
      MacroSegment{
        case trees@Trees(controller, _) =>
          import c.universe._
          
          val spawns = raw.spawns.flatMap{
            case Raw.SpawnDefs(defs) => defs.map{
              case Raw.SingleSpawnDef(name, count) => q"$name -> $count"
            }
          }
          val dur = raw.time.flatMap(_.mp).toMap
          def timeout(tr: c.Expr[FiniteDuration]) = q"akka.util.Timeout($tr)"

          trees.copy(controller = controller.append.body(
            q"protected val spawns: Map[String, Int] = Map(..$spawns)",
            q"""
              import feh.tec.agents.lite.impl.NegotiationEnvironmentController._
              protected lazy val timeouts: Timeouts = new Timeouts {
                lazy val initialize = ${dur.get("initialize").map(timeout) getOrElse q"DefaultTimeouts.initialize"}
                lazy val start = ${dur.get("start").map(timeout) getOrElse q"DefaultTimeouts.start"}
                lazy val stop = ${dur.get("stop").map(timeout) getOrElse q"DefaultTimeouts.stop"}
                lazy val reset = ${dur.get("reset").map(timeout) getOrElse q"DefaultTimeouts.reset"}
                lazy val `response delay` = ${dur.getOrElse("response delay", c.Expr(q"DefaultTimeouts.`response delay`"))}
                lazy val `confirm finished` = ${dur.getOrElse("confirm finished", c.Expr(q"DefaultTimeouts.`confirm finished`"))}
            }"""
          ))
      }

    )
  )

  def ControllerSegmentEmbedControllerDefs(raw: NegotiationRaw) = MacroSegmentsTransform(
    _.append(ControllerBuildingStages.EmbedControllerDefinitions,
      MacroSegment{
        case trees@Trees(controller, _) =>
          import c.universe._

          val newController = controller.append.body(
            q"""def negotiationFinished(neg: NegotiationId, values: Seq[Map[Var, Any]]): Unit = {
             ..${
                  raw.controller.finished
                    .map(t => {
                    q"$t(implicitly[feh.tec.agents.lite.impl.NegotiationEnvironmentController])(neg, values)"
                  })
                    .getOrElse(q"???")
                }
            }""",
            q"""def negotiationFailed(neg: NegotiationId, reason: String): Unit = {
            ..${
                  raw.controller.failed
                    .map(t => q"$t(implicitly[feh.tec.agents.lite.impl.NegotiationEnvironmentController])(neg, reason)")
                    .getOrElse(q"???")
                }
            }"""
          )

          trees.copy(controller = newController)
      }
    )
  )

  def ControllerSegmentSupportBundle = MacroSegmentsTransform(
    _.append(ControllerBuildingStages.Extra,
      MacroSegment{
        case trees@Trees(controller, _) =>
          import c.universe._

          val newController = if(controller.parents.exists(_ <:< typeOf[impl.service.SupportBundle]))
            controller.prepend.body(
              q"def configInfo = feh.tec.agents.lite.impl.service.SupportBundle.Config(${Configs.controller[c.type](c)})"
            )
          else controller

          trees.copy(controller = newController)
      }
    )
  )

}
