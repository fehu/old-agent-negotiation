package feh.tec.web.common

trait WebSocketMessages{
  trait Msg
}

object NQueenMessages extends WebSocketMessages{


  case class Queen(n: Int)
  case class StateReport(of: Queen,
                         position: (Int, Int),
                         priority: Int,
                         proposalAcceptance: Seq[(Queen, Boolean)]
                          ) extends Msg
  case class MessageSent(by: Queen, to: Queen, msg: Message) extends Msg

  case class Message(priority: Int, position: (Int, Int), tpe: MessageType)
  sealed trait MessageType
  case object Proposal extends MessageType
  case object Acceptance extends MessageType
  case object Rejection extends MessageType

}
