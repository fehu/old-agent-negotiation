package feh.tec.agents.impl

import java.util.Date

import akka.actor.ActorLogging
import feh.tec.agents.Message.AutoId
import feh.tec.agents._
import feh.tec.agents.impl.Agent.{AgentReporting, SystemSupport}
import feh.tec.agents.impl.ProposalEngine.SharingKnowledge.SolutionProvenFailure
import feh.util._

import scala.collection.mutable

/** Handles updates of negotiation values and `currentProposal` state
 *  actual proposal creation is defined in [[feh.tec.agents.ProposalBased]]
 */
trait ProposalEngine[Lang <: ProposalLanguage] extends NegotiationStateSupport{
  self: NegotiatingAgent with ProposalBased[Lang] =>

  type StateOfNegotiation <: ProposalNegotiationState[Lang]

  /** resets values and sets a new proposal */
  def resetProposal(neg: ANegotiation): Option[Lang#Proposal]

  /** sets next values set from the common domain and updates proposal */
  def setNextProposal(neg: ANegotiation): Option[Lang#Proposal]
}

trait ProposalNegotiationState[Lang <: ProposalLanguage] extends NegotiationState{
  var currentProposal: Option[Lang#Proposal] = None
  var currentProposalDate: Option[Date] = None
  var currentProposalUnconditionallyAccepted = false
}

trait ProposalIteratorNegotiationState[Lang <: ProposalLanguage] extends ProposalNegotiationState[Lang]{
  var currentIterator: Option[Iterator[Map[Var, Any]]] = None
}

trait ProposalViewState extends NegotiationState{
  var lastWeightedProposal: Option[Map[Option[Boolean], InUnitInterval]] = None
}

object ProposalEngine{
  trait Iterating[Lang <: ProposalLanguage] extends ProposalEngine[Lang]{
    self: NegotiatingAgent with ProposalBased[Lang] =>

    def domainIterators: Map[Var, DomainIterator[Var#Domain, Var#Tpe]]
    
    type StateOfNegotiation <: ProposalIteratorNegotiationState[Lang]

    implicit class VarWrapper[V <: Var](v: V){
      def it(implicit it: DomainIterator[V#Domain, V#Tpe]) = (v, it).asInstanceOf[(Var, DomainIterator[Var#Domain, Var#Tpe])]
    }

  }

  /** Iterates over the domains values using DomainIterator.overSeq
    * ignores initial values
    */
  trait IteratingAllDomains[Lang <: ProposalLanguage] extends Iterating[Lang] with ActorLogging{
    self: NegotiatingAgent with ProposalBased[Lang] with ProposalRegister[Lang] =>

    protected val domainSeqIterators = negotiations.map{ neg => neg.id -> iteratorForNegotiation(neg) }.toMap

    protected def iteratorForNegotiation(ng: Negotiation) = {
      val dItByVar = ng.currentValues.keys.toSeq.zipMap(domainIterators)
      val domains = dItByVar.map(_._1.domain)
      log.info(s"iteratorForNegotiation domains: $domains")
      val it = () => DomainIterator overSeq dItByVar.map(_._2) apply domains
      val vars = dItByVar.map(_._1)
      val i2i: Seq[Any] => Map[Var, Any] = seq => {
        assert(vars.length == seq.length)
        vars.zip(seq).toMap
      }
      it -> i2i
    }

    protected def newIterator(negId: NegotiationId): Iterator[Map[Var, Any]] = {
      val (dIt, toIssues) = domainSeqIterators(negId)
      val it = dIt()
      it map toIssues
    }

    protected def nextIssues(neg: Negotiation) =  neg.state.currentIterator flatMap {
      _.inCase(_.hasNext).map(_.next())
    }

    /** resets values and sets a new proposal */
    def resetProposal(neg: ANegotiation) = {
      neg.state.currentIterator = Some(newIterator(neg.id))
      val it = neg.state.currentIterator.get
      setNextProposal(neg)
    }

    /** sets next values set from the common domain and updates proposal */
    def setNextProposal(neg: ANegotiation) = {
      neg.state.currentProposalUnconditionallyAccepted = false
      neg.currentValuesAcceptance = false

      nextIssues(neg).map{
        issues =>
          neg.currentValues ++= issues
          updateProposal(neg)
      }
    }

    protected implicit def issuesExtractor: IssuesExtractor[Lang]

    protected def updateProposal(neg: ANegotiation) = {
      val old = neg.state.currentProposal
      val prop = createProposal(neg.id)
      neg.state.currentProposal.foreach( _.id |> discardProposal )  // discard the old proposal in the register
      neg.state.currentProposal = Option(prop)                      // when a new one is set
      neg.state.currentProposalDate = Some(new Date())
      prop
    }
  }
  
  trait IteratingCurrentIssues[Lang <: ProposalLanguage] extends Iterating[Lang]{ // todo
    self: NegotiatingAgent with ProposalBased[Lang] =>
  }

  trait SolutionValues{
    def values: Map[AgentRef, Map[Var, Any]]
    def pureValues = values.values.toSet

    override def equals(obj: scala.Any) = PartialFunction.cond(obj) {
      case that: SolutionValues => this.values == that.values
    }
    override def hashCode() = values.hashCode()
  }
  
  object SolutionValues{
    def apply(vals: Map[AgentRef, Map[Var, Any]]) = 
      new SolutionValues { 
        def values = vals
      }
    
    implicit class MineExt(c: SolutionValues){
      def my(implicit ref: AgentRef) = MySolutionValues(c.values(ref), c.values - ref)
    }
  }
  
  case class MySolutionValues(myValues: Map[Var, Any], othersValues: Map[AgentRef, Map[Var, Any]])
                                  (implicit ref: AgentRef) extends SolutionValues
  {
    def values = othersValues + (ref -> myValues)
  }

  // todo: shall be refactored
  /** Guards values configuration that were proved failure and avoids them in the future */
  trait LearningFromMistakes[Lang <: ProposalLanguage] extends ProposalEngine[Lang]{
    self: NegotiatingAgent with ProposalBased[Lang] with ActorLogging =>

    protected type MyConf = Map[Var, Any]

    protected val failedConfigurations = negotiations.map(neg =>
      neg.id -> mutable.HashSet.empty[MySolutionValues]
    ).toMap

    def configurationFailed(neg: NegotiationId): MySolutionValues =
      currentConfiguration(neg) $$ { failedConfigurations(neg) += _ }

    def provenFailure(neg: NegotiationId, takeAgentInAccount: Boolean = false)(myConf: MyConf): Boolean = {
      val conf = currentConfiguration(neg).copy(myValues = myConf)
      if(takeAgentInAccount) failedConfigurations(neg).exists(conf ==)
      else failedConfigurations(neg).map(_.pureValues).exists(conf.pureValues ==)
    } 
      

    def notProvenFailure(neg: NegotiationId, takeAgentInAccount: Boolean = false)(myConf: MyConf) = !provenFailure(neg, takeAgentInAccount)(myConf)

    protected def currentConfiguration(negId: NegotiationId): MySolutionValues
  }

  /** Avoids value configurations proven failure */
  trait IteratingAllDomainsLearningFromMistakes[Lang <: ProposalLanguage] 
    extends IteratingAllDomains[Lang] with LearningFromMistakes[Lang]
  {
    self: NegotiatingAgent with ProposalBased[Lang] with ProposalRegister[Lang] with SpeakingAgent[_] with AgentHelpers[_] =>

    def takeAgentInAccountDuringFailedProposalFiltering = false

    override protected def nextIssues(neg: Negotiation) = neg.state.currentIterator flatMap {
      _.filter(notProvenFailure(neg.id, takeAgentInAccountDuringFailedProposalFiltering)).inCase(_.hasNext).map(_.next())
    }
  }


  object SharingKnowledge{
    case class SolutionProvenFailure(neg: NegotiationId, solution: SolutionValues)
                                    (implicit sender: AgentRef) extends SystemMessage with AutoId
    {
      def senderId = sender.id
    }
  }

  trait SharingKnowledge[Lang <: ProposalLanguage] extends LearningFromMistakes[Lang] with SystemSupport{
    self: NegotiatingAgent
            with ProposalBased[Lang]
            with ActorLogging
//            with SpeakingAgent[Lang]
            with AgentHelpers[Lang] =>1
//            with AgentReporting[Lang] =>

    def knowledgeShare: AgentRef

    override def configurationFailed(neg: NegotiationId): MySolutionValues = {
      val cf = super.configurationFailed(neg)
      val msg = SolutionProvenFailure(neg, cf)
      knowledgeShare.ref ! msg
//      reportingTo.ref ! msg
      cf
    }

    override def processSys = super.processSys orElse{
      case SolutionProvenFailure(neg, config) => failedConfigurations(neg) += config.my
    }
  }
}