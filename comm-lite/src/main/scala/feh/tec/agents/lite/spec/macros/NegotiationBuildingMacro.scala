package feh.tec.agents.lite.spec.macros

import feh.tec.agents.lite.impl.NegotiationEnvironmentController
import feh.tec.agents.lite.spec
import feh.tec.agents.lite.spec.NegotiationSpecification.Interlocutors
import feh.tec.agents.lite.spec.{AgentSpecification, NegotiationSpecification}

import scala.concurrent.duration.FiniteDuration
import scala.reflect.macros.whitebox

trait NegotiationBuildingMacro[C <: whitebox.Context] extends MacroContext[C]{

  case class NegotiationRaw(vars: List[Raw.VarDef],
                            negotiations: List[Raw.NegotiationDef],
                            agents: List[Raw.AgentDef],
                            spawns: List[Raw.SpawnDefs],
                            time: List[Raw.TimeDefs],
                            controller: Raw.ControllerDefs
                             )


  object Raw{
    type NegotiationDef = NegotiationSpecification.NegotiationDef

    case class VarDef(name: String, domain: DomainDef)
    case class DomainDef(domain: c.Tree, tpe: c.Type, domTpe: c.Type, domSize: c.Tree)

    case class SingleSpawnDef(name: String, count: c.Expr[Int])
    case class SpawnDefs(defs: Seq[SingleSpawnDef])

    case class AgentDef(name: String,
                        role: String,
                        negotiations: Seq[AgentNegDef],
                        spec: c.Expr[AgentSpecification])

    case class AgentNegDef(negotiation: String,
                           interlocutors: Interlocutors,
                           interlocutorsExpr: c.Expr[Interlocutors],
                           reportingToOpt: Option[c.Tree],
                           constraints: Seq[AgentConstraintsDef])
    case class AgentConstraintsDef(constraints: Seq[c.Tree])

    case class TimeDefs(mp: Map[String, c.Expr[FiniteDuration]])

    case class ControllerDefs(finished: Option[c.Expr[NegotiationEnvironmentController => (String, Seq[Map[String, Any]]) => Any]],
                              failed: Option[c.Expr[NegotiationEnvironmentController => (String, String) => Any]])
  }

  def build(dsl: c.Expr[spec.dsl.Negotiation]): NegotiationRaw

}
