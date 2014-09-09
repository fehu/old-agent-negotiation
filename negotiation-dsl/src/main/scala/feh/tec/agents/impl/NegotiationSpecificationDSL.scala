package feh.tec.agents.impl

import scala.concurrent.duration.FiniteDuration
import scala.language.experimental.macros

trait NegotiationSpecificationDSL extends ConstraintsSpecificationDSL with CreationSpecificationDSL{
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
    type ChooseWith = {
      def `with`[S: SelectsInterlocutors](interlocutors: S*): AgentNegPartialDef
    }

    def the(neg: String): ChooseWith
    def the(neg: NegotiationDef): ChooseWith
  }
  sealed trait SelectsInterlocutors[S]

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

  def domain[C[_], T](subj: C[T]): DomainDef[T] = stub
  def domain(r: Range): DomainDef[Int] = stub

  def hasConstraints(c: AgentConstraint*): AgentNegPartialDef = stub

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

trait CreationSpecificationDSL{
  self: NegotiationSpecificationDSL =>

  import NegotiationSpecification._

  protected type TimeoutIdent = {
    def <= (t: FiniteDuration): TimeoutDef
  }

  protected trait ConfDef
  protected trait TimeoutDef extends ConfDef

  type ChooseTimeout = {
    def creation: TimeoutIdent
    def startup: TimeoutIdent
    def `resolve conflict`: TimeoutIdent
  }
  
  def spawn:{
    def agents(count: (AgentDef, Int))
    def agents(count: (String, Int))
    def agent(ag: AgentDef)
  } = stub
  
  def configure(c: ConfDef*) = stub
  def timeout: ChooseTimeout = stub

  implicit class TimingDefWrapper(f: ChooseTimeout => TimeoutIdent){
    def >>(time: FiniteDuration): TimeoutDef = stub
  }
}

trait ConstraintsSpecificationDSL{
  self:  NegotiationSpecificationDSL =>

  import NegotiationSpecification._

  def proposed[T] (vr: AbstractVarDef[T]): T = stub
  def valueOf[T]  (vr: AbstractVarDef[T]): T = stub


  implicit class VarDefConstraintBuilder[T](name: String){
    def |(withWrapper: Boolean): AgentConstraint = stub
  }

}
