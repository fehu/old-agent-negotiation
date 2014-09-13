package feh.tec.web

import feh.tec.web.common.NQueenMessages
import org.scalajs.dom.{WebSocket, document}
import org.scalajs.jquery.jQuery
import scala.scalajs.js.Dynamic.global

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.{JSON, JSApp}

object NQueen extends JSApp with NQueenSocketListener with NQueenTemplates{

  def host = "ws://localhost:8080"

//  def appendPar(target: dom.Node, text: String): Unit ={
//    jQuery(target).append("<p>"+text+"</p>")
//  }

  lazy val board = new ChessBoard(4)

  def main() = {
    jQuery(document.body) append board.create
  }
}


trait NQueenTemplates{
  
  def chessBoardSel = jQuery(".chess-board")
  
  class ChessBoard(size: Int) {
    def create = s"""
      <table class="chess-board">
        ${(0 to size).map{
          i => s"""<tr>${ (0 to size).map{
            case 0 if i == 0  => "<td class=\"description\"></td>"
            case j if i == 0  => s"""<td class="description">$j</td>"""
            case 0            => s"""<td class="description">$i</td>"""
            case _            => "<td></td>"
          }.mkString("") }</tr>"""
        }.mkString("\n")}
      </table>"""
    
    protected val placedQueens = mutable.HashMap.empty[Int, (Int, Int)] // queen's number -> position
    
    def updatePositions(posUpdate: Map[Int, (Int, Int)]) = {
      placedQueens ++= posUpdate.toSeq
      val newLabels = placedQueens.groupBy(_._2).mapValues(_.keySet).mapValues(_.mkString(",")).withDefaultValue("")
      (1 to size) zip (1 to size) map {
        case p@(i, j) =>
          jQuery(s".chess-board tr:nth-child($i) td:nth-child($j)") text newLabels(p)
      }
    }
  }
}


trait NQueenSocketListener extends SocketConnections{
  self: NQueenTemplates =>

  def host: String

  protected lazy val sockets = new WebSocket(host) :: Nil

  def onMessage: PartialFunction[js.Any, Unit] = {
    case msg: js.prim.String =>
      println(msg)
      val json = JSON.parse(msg)
      global.console.log(json)
      json.$t.asInstanceOf[String] match {
        case "StateReport" =>
          println("!!! StateReport !!!")
      }
  }
}