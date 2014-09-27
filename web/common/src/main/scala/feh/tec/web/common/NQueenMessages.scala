package feh.tec.web.common

import java.util.UUID

import feh.tec.web.common.NQueenMessages.Queen

trait WebSocketMessages{
  trait Msg
  trait CanBulk extends Msg {
    def at: Int // time from negotiation start in ms
    def by: Queen
    def reportsState: Boolean
    def reportsMessage: Boolean
  }
}

object NQueenMessages extends WebSocketMessages{


  case class Queen(n: Int)
  case class Init(queens: Seq[(Queen, String)]) extends Msg
  case class StateReport(by: Queen,
                         position: (Int, Int),
                         priority: Int,
                         proposalAcceptance: Seq[(Queen, Boolean)],
                         at: Int, // system time in millis
                         acceptanceFlag: Boolean
                          ) extends CanBulk
  {
    def reportsState = true
    def reportsMessage = false

    def id = at -> by
  }
  case class MessageReport(by: Queen, to: Queen, msg: Message, at: Int, extra: Option[MessageExtraReport]) extends CanBulk
  {
    def reportsState = false
    def reportsMessage = true

    def id = msg.id
  }
  case class BulkReport(messages: Seq[CanBulk]) extends Msg

  case class Message(id: String, priority: Int, content: MessageContent){
    def isProposal = content.isInstanceOf[Proposal]
  }
  sealed trait MessageContent
  case class Proposal(position: (Int, Int)) extends MessageContent
  case class Response(proposal: String, tpe: MessageType) extends MessageContent
  object Response{
    def apply(proposal: UUID, tpe: NQueenMessages.type => MessageType): Response = Response(proposal.toString, tpe(NQueenMessages))
  }

  sealed trait MessageType
  case object Acceptance extends MessageType
  case object Rejection extends MessageType

  trait MessageExtraReport
  
  case class ReportWeight(weight: Seq[(Option[Boolean], Double)]) extends MessageExtraReport

  case object NegotiationFinished extends Msg
}
