package feh.tec.agents.lite.impl.agent

import feh.tec.agents.lite.Message.ProposalId
import feh.tec.agents.lite._
import feh.util._
import feh.tec.agents.lite.impl.FailedConfigurationsChecks
import scala.collection.mutable

trait FailureChecks[Lang <: Language.ProposalBased with Language.HasPriority]
  extends FailedConfigurationsChecks[Lang] with AgentHelpers[Lang]
{
  private var failureCheckFuncs =
    Map.empty[(NegotiationId, Map[Var, Any]), mutable.HashMap[PartialValuesConfiguration, Map[Priority, Map[Var, Any]] => Option[Boolean]]]
  private var failureCheckEvidences =
    Map.empty[NegotiationId, (ProposalId, Map[Priority, Map[Var, Any]])]

  protected def failureFuncsFor(neg: NegotiationId, myValues: Map[Var, Any]) =
    failureCheckFuncs.get(neg -> myValues).map(_.toMap.values) getOrElse Nil

  protected def updateFailureCheckFuncs(failed: PartialValuesConfiguration,
                                        vals: Map[Var, Any],
                                        func: Map[Priority, Map[Var, Any]] => Option[Boolean]): Unit =
  {
    val confs = failureCheckFuncs.getOrElse(
      failed.negotiation -> vals,
      mutable.HashMap.empty[PartialValuesConfiguration, Map[Priority, Map[Var, Any]] => Option[Boolean]] $$ (failureCheckFuncs += (failed.negotiation, vals) -> _)
    )
    confs += failed -> func
  }

  /** yes / no / None = maybe */
  def repeatingAFailure(acceptance: Lang#Acceptance): Option[Boolean] = {
    log.debug("repeatingAFailure?")
    val evidenceOpt = failureCheckEvidences.get(acceptance.negotiation).filter(_._1 == acceptance.respondingTo)
    val evidence = evidenceOpt map{
      case (id, vals) =>
        val newEv = vals.ensuringNot(_.contains(acceptance.priority)) + (acceptance.priority -> acceptance.myValues)
        failureCheckEvidences += acceptance.negotiation -> (id, newEv)
        newEv
    } getOrElse{
      val newEv = Map(acceptance.priority -> acceptance.myValues)
      failureCheckEvidences += acceptance.negotiation -> (acceptance.respondingTo, newEv)
      newEv
    }

    val failOpt = failureFuncsFor(acceptance.negotiation, get(acceptance.negotiation).currentValues()).map(_(evidence))
    log.debug("repeatingAFailure? " + failOpt)
    if(failOpt.exists(_.contains(true))) Some(true)
    else if (failOpt.exists(_.isEmpty)) None
    else Some(false)
  }

  /** Guards functions for checking the configuration given */
  def guardFailedConfiguration(failed: PartialValuesConfiguration): Unit = {

    def failure(evidence: Map[Priority, Map[Var, Any]], myValues: Map[Var, Any], myPriority: Priority): Option[Boolean] = {
      val (mentionsMe, withoutMe) = failed.configurations.partition{ case (_, `myValues`) => true }
      assert(mentionsMe.size == 1)
      if(withoutMe.forall(_._1 > myPriority)){
        val satisfies = withoutMe.map{
          case (p, vals) =>  evidence.get(p).map(_ == vals)
        }
        if(satisfies.exists(_.isEmpty)) None
        else if(satisfies.exists(_.contains(false))) Some(false)
        else Some(true)
      }
      else Some(false)
    }

    for {
      (_, vals) <- failed.configurations
    } updateFailureCheckFuncs(failed, vals, failure(_, vals, get(failed.negotiation).currentPriority()))
  }
}
