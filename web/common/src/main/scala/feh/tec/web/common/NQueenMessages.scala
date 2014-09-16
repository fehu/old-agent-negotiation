package feh.tec.web.common

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
                         at: Int // system time in millis
                          ) extends CanBulk
  {
    def reportsState = true
    def reportsMessage = false
  }
  case class MessageReport(by: Queen, to: Queen, msg: Message, at: Int) extends CanBulk
  {
    def reportsState = false
    def reportsMessage = true
  }
  case class BulkReport(messages: Seq[CanBulk]) extends Msg

  case class Message(priority: Int, position: (Int, Int), tpe: MessageType)
  sealed trait MessageType
  case object Proposal extends MessageType
  case object Acceptance extends MessageType
  case object Rejection extends MessageType

}
