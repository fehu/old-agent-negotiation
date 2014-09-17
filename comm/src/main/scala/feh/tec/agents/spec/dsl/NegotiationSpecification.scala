package feh.tec.agents.spec.dsl

trait NegotiationSpecification extends ConstraintsSpecification with CreationSpecification{
  import feh.tec.agents.spec.NegotiationSpecification._

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
