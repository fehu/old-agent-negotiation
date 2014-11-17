package feh.tec.agents.light.impl

import akka.actor.ActorLogging
import feh.tec.agents.light.spec.NegotiationSpecification._
import feh.tec.agents.light.{AgentHelpers, Var, NegotiatingAgent, NegotiationId}
import feh.util._

trait ConstraintsSatisfactionChecks {
  self: NegotiatingAgent[_] with ActorLogging with AgentHelpers[_] =>

  val constraintsByNegotiation: Map[NegotiationId, AgentConstraintsDef]

  def satisfiesConstraints(of: NegotiationId, values: Map[Var, Any]): Boolean = constraintsByNegotiation(of).constraints forall {
    case AgentConstraintDef(cName, parts) =>
      val neg = get(of)
      val myValues = neg.currentValues()
      log.debug(s"satisfiesConstraints values=$values, myValues=$myValues, neg=$neg")
      satisfiesParts(parts, values, myValues) $$
        { r => log.debug(s"constrain $cName ${if(r) "" else "NOT "}satisfied") }
  }

  protected def ifConstraintLeafForOtherVars = true

  /** should be generated in macro */
  protected def seqToTuple(seq: Seq[Any]): Product

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
        log.debug(s"testArgs = $testArgs")
        test(seqToTuple(testArgs))
      case _: ConstraintPartLeaf[_] => ifConstraintLeafForOtherVars
    }
  }
}
