package feh.tec.agents.lite.impl

import akka.actor.ActorLogging
import feh.tec.agents.lite.spec.NegotiationSpecification._
import feh.tec.agents.lite.{AgentHelpers, Var, NegotiatingAgent, NegotiationId}
import feh.util._

trait ConstraintsSatisfactionChecks {
  self: NegotiatingAgent[_] with ActorLogging with AgentHelpers[_] =>

  val constraintsByNegotiation: Map[NegotiationId, AgentConstraintsDef]

  def satisfiesConstraints(of: NegotiationId, values: Map[Var, Any]): Boolean = constraintsByNegotiation(of).constraints forall {
    case AgentConstraintDef(cName, parts) =>
      val res = satisfiesParts(parts, values, get(of).currentValues())
      log.debug(s"Constraint($cName): $res for values=$values, currentValues=${get(of).currentValues()}")
      res
  }

  protected def ifConstraintLeafForOtherVars = true

  /** should be generated in macro */
  protected def seqToTuple(seq: Seq[Any]): Product

  // todo: up to 14% of CPU and 17% of MEM
  protected def satisfiesParts(part: ConstraintPart[AgentVarConstraintDef], proposed: Map[Var, Any], myValues: Map[Var, Any]): Boolean = {
    def recCall(p: ConstraintPart[AgentVarConstraintDef]): Boolean = satisfiesParts(p, proposed, myValues)
    part match {
      case ConstraintPartComb("and", left, right) => recCall(left) && recCall(right)
      case ConstraintPartComb("or", left, right) => recCall(left) || recCall(right)
      case ConstraintPartLeaf(AgentVarConstraintDef(vars, test)) if vars.map(_.varName).forall(vn => proposed.exists(_._1.name == vn)) =>
        val testArgs = vars map{
          case ConstraintParamDescription("proposed", varName, _) =>
            proposed.find(_._1.name == varName).getOrThrow(s"no var $varName found among proposed $proposed")._2
          case ConstraintParamDescription("valueOf", varName, _) =>
            myValues.find(_._1.name == varName).getOrThrow(s"no var $varName found among my values $myValues")._2
        }
        test(seqToTuple(testArgs))
      case _: ConstraintPartLeaf[_] => ifConstraintLeafForOtherVars
    }
  }
}
