package feh.tec.agents.light

import feh.tec.agents.light.SystemMessage.ScopeUpdate
import feh.util.AbstractScopedState

trait AgentHelpers[Lang <: NegotiationLanguage]{
  self: NegotiatingAgent[Lang] with SpeakingAgent[Lang] =>

  protected object hooks{

    object OnSend extends AbstractScopedState[Set[(AgentRef, Lang#Msg) => Unit]]{
      private var hooks = default

      protected def default = Set()
      def get = hooks
      protected def state_=(t: Set[(AgentRef, Lang#Msg) => Unit]) = hooks = t

      def withHooks[R](hooks: ((AgentRef, Lang#Msg) => Unit)*)(f: => R) = {
        val old = get
        this.hooks ++= hooks
        val res = f
        this.hooks = old
        res
      }
    }

  }

  protected abstract class AgentRefWrapper(ref: AgentRef){
    def !(msg: Lang#Msg)
  }

  implicit def agentRefWrapper(ref: AgentRef): AgentRefWrapper = new AgentRefWrapper(ref) {
    def !(msg: Lang#Msg) = {
      ref.ref ! msg
      hooks.OnSend.get foreach (_(ref, msg))
    }
  }

  def sendToAll(msg: Lang#Msg) = get(msg.negotiation).scope().foreach(_ ! msg)

  private val negotiationsCache = negotiations.map(n => n.id -> n).toMap
  def get(neg: NegotiationId): Negotiation = negotiationsCache(neg)
  def getOpt(neg: NegotiationId) = negotiationsCache.get(neg)
}

trait SystemSupport extends AbstractAgent{

  def processSys: PartialFunction[SystemMessage, Any] = {
    case SystemMessage.Initialize => initialize();  sender() ! SystemMessage.Initialized
    case SystemMessage.Start      => start();       sender() ! SystemMessage.Started
    case SystemMessage.Stop       => stop();        sender() ! SystemMessage.Stopped
    case SystemMessage.Reset      => reset();       sender() ! SystemMessage.Reset
  }

  def initialize()
  def start()
  def stop()
  def reset()
}

trait SpeakingSystemSupport[Lang <: Language] extends SystemSupport{
  self: SpeakingAgent[Lang] =>

  def receive = {
    case sys: SystemMessage => processSys(sys)
    case msg: Lang#Msg      => beforeEachMessage(msg); process(msg)
  }
  
  protected def beforeEachMessage(msg: Lang#Msg)
}

trait DynamicScopeSupport[Lang <: NegotiationLanguage]
  extends SystemSupport
  with NegotiatingAgent[Lang]
  with AgentHelpers[Lang]
{
  type Negotiation <: Negotiation.DynamicScope

  override def processSys = super.processSys orElse {
    case ScopeUpdate.NewScope(scope, neg) => get(neg).scope update scope;       get(neg).scopeUpdated()
    case ScopeUpdate.NewAgents(ag, neg)   => get(neg).scope update (_ ++ ag);   get(neg).scopeUpdated()
    case ScopeUpdate.RmAgents(ag, neg)    => get(neg).scope update (_ -- ag);   get(neg).scopeUpdated()
  }
}