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
  case class ChangeReport(by: Queen,
                          at: Int, // system time in millis
                          position: Option[(Int, Int)],
                          state: Option[String]
                          ) extends CanBulk
  {
    def reportsState = true
    def reportsMessage = false

    def id = at -> by
  }
  @deprecated("must guard values as map")
  case class StateReport(by: Queen,
                         position: (Int, Int),
                         priority: Int,
                         proposalAcceptance: Seq[(Queen, Boolean)],
                         at: Int, // system time in millis
                         acceptanceFlag: Boolean,
                         topPriorityFlag: Boolean
                          ) extends CanBulk
  {
    def reportsState = true
    def reportsMessage = false

    def id = at -> by
  }
  case class MessageReport(by: Queen, to: Queen, msg: Message, extra: Option[MessageExtraReport]) extends CanBulk
  {
    def reportsState = false
    def reportsMessage = true
    def at: Int = msg.at

    def id = (at, by, to)
  }
  case class BulkReport(messages: Seq[CanBulk]) extends Msg

  case class Message(at: Int, priority: Int, content: MessageContent){
    def isProposal = content.isInstanceOf[Proposal]
  }
  sealed trait MessageContent
  case class Proposal(id: String, position: (Int, Int)) extends MessageContent
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
  case class NegotiationFinishedAutoRestart(delay: Int) extends Msg
  case object Restart extends Msg

  case class PositionProvenFailure(pos: (Int, Int)) extends Msg
}
