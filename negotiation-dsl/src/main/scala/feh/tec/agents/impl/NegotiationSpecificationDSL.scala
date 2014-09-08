package feh.tec.agents.impl

import scala.language.experimental.macros

trait NegotiationSpecificationDSL extends ConstraintsSpecificationDSL{
  import NegotiationSpecification._

  type ChooseVarDomain = {
    def `with`[T](domain: DomainDef[T]): AbstractVarDef[T]
  }

  type ChooseNegotiationOver = {
    def over(vars: String*): NegotiationDef
    def over(vars: AbstractVarDef[_]*): NegotiationDef
  }

  type ChooseAgentRole = {
    def withRole(role: String): {
      def that(negDefs: AgentNegPartialDef*): AgentDef
    }
  }

  type ChooseAgentNegotiation = {
    def the(neg: String): {
      def `with`[S: SelectsInterlocutors](interlocutors: S*): AgentNegPartialDef
    }
  }
  sealed trait SelectsInterlocutors[S]

  type ChooseAgentConstraints = {
    def over(c: AgentConstraint*): AgentNegPartialDef
  }
  protected trait AgentConstraint

  def define = new {
    def variable(name: String)    : ChooseVarDomain = stub
    def negotiation(name: String) : ChooseNegotiationOver = stub
    def agent(name: String)       : ChooseAgentRole = stub
  }
  def variable          : ChooseVarDomain = stub
  def negotiation       : ChooseNegotiationOver = stub
  def agent             : ChooseAgentRole = stub
  def negotiates        : ChooseAgentNegotiation = stub
  def hasConstraints    : ChooseAgentConstraints = stub


  def domain[C[_], T](subj: C[T]): DomainDef[T] = stub
  def domain(r: Range): DomainDef[Int] = stub

  case object neighbours
  protected case object TheOthers
  def the: { 
    def others: TheOthers.type
  } = stub

  implicit object StringSelectsInterlocutorsByRole extends SelectsInterlocutors[String]
  implicit object NeighboursSelectsInterlocutors extends SelectsInterlocutors[neighbours.type]
  implicit object TheRestOfSelectsInterlocutors extends SelectsInterlocutors[TheOthers.type]

  //  implicit def extendedConstraintIsAgentConstraint[T](constraintFunc: (T, T) => Boolean): AgentConstraint = stub


  protected def stub = sys.error("this method should never be called")
}

trait ConstraintsSpecificationDSL{
  self:  NegotiationSpecificationDSL =>

  import NegotiationSpecification._
  import ConstraintsSpecificationDSL.CW

  def proposed  = ConstraintsSpecificationDSL.proposed
  def value     = ConstraintsSpecificationDSL.value
  def my        = ConstraintsSpecificationDSL.my


  implicit class VarDefConstraintBuilder[T](buildingFor: AbstractVarDef[T]){
    def >>(withWrapper: CW[T] => Boolean): AgentConstraint = stub
  }

}

object ConstraintsSpecificationDSL{
  protected class CW[T] private (f: CBuilderKey => T) extends (CBuilderKey => T){
    def apply(v1: CBuilderKey): T = ???
  }

  sealed trait CBuilderKey
  object proposed extends CBuilderKey
  object value extends CBuilderKey
  object my{
    def current[T](v: value.type)(implicit wrap: CW[T]) = wrap(v)
  }
}