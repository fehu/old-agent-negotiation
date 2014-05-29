package feh.agents.comm

import feh.agents.comm.Message.MessageId

/** Language meta descriptor
 */
trait Language {
  def name: String
  type Expr
  def exprClass: Class[Expr]
}

//trait NegotiatingLanguage extends Language{
//  type Proposal <: Expr
//}

trait LanguageExprBuilder[+Lang <: Language] // todo: not sure if really needed

trait NegotiationExprBuilder[+Lang <: Language] extends LanguageExprBuilder[Lang]{
  def accepted: Lang#Expr
}

trait NegotiationProposalExprBuilder[+Lang <: Language, -ISpace <: IssuesSpace] extends NegotiationExprBuilder[Lang]{
  def proposal(prop: ISpace#Point): Lang#Expr
  def counterProposal(originalProp: MessageId, counter: ISpace#Point): Lang#Expr
}