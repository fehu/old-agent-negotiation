package feh.tec.web.common

trait WebSocketMessages{
  trait Msg
  trait CanBulk extends Msg
}

object NQueenMessages extends WebSocketMessages{


  case class Queen(n: Int)
  case class Init(queens: Seq[(Queen, String)]) extends Msg
  case class StateReport(of: Queen,
                         position: (Int, Int),
                         priority: Int,
                         proposalAcceptance: Seq[(Queen, Boolean)]
                          ) extends CanBulk
  case class MessageReport(by: Queen, to: Queen, msg: Message) extends CanBulk
  case class BulkReport(messages: Seq[CanBulk]) extends Msg

  case class Message(priority: Int, position: (Int, Int), tpe: MessageType)
  sealed trait MessageType
  case object Proposal extends MessageType
  case object Acceptance extends MessageType
  case object Rejection extends MessageType

}
