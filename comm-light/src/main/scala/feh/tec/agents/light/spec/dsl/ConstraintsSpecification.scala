package feh.tec.agents.light.spec.dsl

trait ConstraintsSpecification{
  self:  NegotiationSpecification =>

  import feh.tec.agents.light.spec.NegotiationSpecification._

  def proposed[T] (vr: AbstractVarDef[T]): T = stub
  def valueOf[T]  (vr: AbstractVarDef[T]): T = stub


  implicit class VarDefConstraintBuilder[T](name: String){
    def |(withWrapper: Boolean): AgentConstraint = stub
  }

}
