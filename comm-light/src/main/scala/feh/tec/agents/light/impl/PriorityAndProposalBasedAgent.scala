package feh.tec.agents.light.impl

import feh.tec.agents.light.Message.ProposalId
import feh.tec.agents.light._
import feh.tec.agents.light.spec.{MonoDefinition, ExtendableDefinition}

class PriorityAndProposalBasedAgent[Lang <: Language.ProposalBased with Language.HasPriority]
  extends PriorityProposalBasedAgent[Lang] with DynamicScopeSupport[Lang] with SpeakingSystemSupport[Lang]
{
  type Negotiation <: Negotiation.DynamicScope with Negotiation.HasPriority with Negotiation.HasProposal[Lang]

  /* should be defined by ExtendableDefinition */

  def nothingToPropose(neg: NegotiationId): Unit = ???

  protected def beforeEachMessage(msg: Lang#Msg): Unit = ???

  def setNextProposal(neg: NegotiationId): Unit = ???

  def nextValues(neg: NegotiationId): Option[Map[Var, Any]] = ???

  def onProposal: PartialFunction[Lang#Proposal, Any] = ???

  def onRejection: PartialFunction[Lang#Rejection, Any] = ???

  def onAcceptance: PartialFunction[Lang#Acceptance, Any] = ???

  def updateCurrentProposal(neg: NegotiationId): Unit = ???

  def priorityNegotiationHandler: PriorityNegotiationHandler[Lang] = ???

  def stop(): Unit = ???

  def reset(): Unit = ???

  def start(): Unit = ???

  /* == == == == Utils == == == == */

  object myPriority{
    def isHigherThenOf(msg: Lang#Msg) = comparePriority(msg, _ > _)
    def isLowerThenOf(msg: Lang#Msg) = comparePriority(msg, _ < _)
  }

  /** (this, that) => Boolean */
  def comparePriority(msg: Lang#Msg, f: (Priority, Priority) => Boolean): Boolean =
    f(get(msg.negotiation).currentPriority(), msg.priority)

  def requestPriorityRaise(neg: NegotiationId): Lang#PriorityRaiseRequest = ???



  def process: PartialFunction[Lang#Msg, Any] = {
    case prop: Lang#Proposal   => onProposal lift prop
    case resp: Lang#Acceptance => onAcceptance lift resp
    case resp: Lang#Rejection  => onRejection lift resp
    case priority: Lang#Priority =>
  }

  val ref: AgentRef = null

  val name: String = null

  def negotiations: Set[Negotiation] = ???

  override val role: NegotiationRole = null
}

object PriorityAndProposalBasedAgent{

  type Ag = PriorityAndProposalBasedAgent[_]

  trait ExtDef[T] extends ExtendableDefinition[Ag, T]
  class MonoDef[T](default: Ag => T) extends MonoDefinition[Ag, T](default) with ExtDef[T]
  class BaseDef[T](default: Ag => T) extends MonoDef[T](default) with ExtendableDefinition.BeforeAndAfter[T]{
    override def extensionPoints = super[MonoDef].extensionPoints ++ super[BeforeAndAfter].extensionPoints
    override def get(implicit owner: PriorityAndProposalBasedAgent[_]): T = {
      BeforeExtension.get
      AfterExtension.get apply DefExtension.get(owner)
    }
  }

  object Default{

    type Lang = Language.ProposalBased with Language.HasPriority

    lazy val nothingToPropose = new ExtDef[NegotiationId => Unit]{
      def get(implicit owner: PriorityAndProposalBasedAgent[_]): NegotiationId => Unit = ???

      def extensionPoints: Map[String, ExtensionEntry[Any]] = ???
    }

    protected lazy val beforeEachMessage = ??? // ExtDef[Lang#Msg => Unit]

    lazy val setNextProposal = ??? // ExtDef[NegotiationId => Unit]

    lazy val nextValues = ??? // ExtDef[NegotiationId => Option[Map[Var, Any]]]

    lazy val onProposal = new MonoDef[PartialFunction[Lang#Proposal, Any]](Map())

    lazy val onRejection = new MonoDef[PartialFunction[Lang#Rejection, Any]](Map())

    lazy val onAcceptance = new MonoDef[PartialFunction[Lang#Acceptance, Any]](Map())

    lazy val updateCurrentProposal = new BaseDef[NegotiationId => Unit](
      owner => {
        negId =>
          val neg = owner.get(negId)
          neg.currentProposal update Message.Proposal(ProposalId.rand, negId, neg.currentValues())
      }
    )

    lazy val requestPriorityRaise = ??? // ExtDef[NegotiationId => Lang#PriorityRaiseRequest]

    lazy val priorityNegotiationHandler = ??? //ExtDef[PriorityNegotiationHandler[Lang] ]

    lazy val stop = ??? // ExtDef[Unit]

    lazy val reset = ??? // ExtDef[Unit]

    lazy val start = ??? // ExtDef[Unit]

  }
}