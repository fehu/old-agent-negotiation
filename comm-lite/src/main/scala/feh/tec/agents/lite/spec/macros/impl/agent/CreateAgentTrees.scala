package feh.tec.agents.lite.spec.macros.impl.agent

import feh.tec.agents.lite.spec.macros.{ControllerBuildingMacro, AgentsBuildingMacroBase}
import scala.reflect.macros.whitebox

/** Contains `MacroSegmentsTransform`s for `CreateAgentTrees` stage
  */
trait CreateAgentTrees[C <: whitebox.Context]{
  self: AgentsBuildingMacroBase[C] with ControllerBuildingMacro[C] =>

  def allCreateAgentTrees(raw: NegotiationRaw, anonAgentClassName: String = "$AgentAnonClass") =
    AgentSegmentEmptyAgentTrees(raw, anonAgentClassName) :: Nil

  /** Creates empty [[ActorTrees]] for every actor, defined in `raw`, assigning them `anonAgentClassName`
    */
  def AgentSegmentEmptyAgentTrees(raw: NegotiationRaw, anonAgentClassName: String = "$AgentAnonClass") = MacroSegmentsTransform{
    _.append(AgentBuildingStages.CreateAgentTrees, {
      case tr if tr.agents.isEmpty =>
        val agents = raw.agents.map(_.name -> ActorTrees(anonAgentClassName, Nil, Nil, Map())).toMap
        tr.copy(agents = agents)
    })
  }
}
