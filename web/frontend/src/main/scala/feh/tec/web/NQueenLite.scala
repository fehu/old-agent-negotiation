package feh.tec.web

import feh.tec.web.QueenInfo.SelectionDisabled

object NQueenLite extends NQueen{
  override def queensInfoCommunicationsInit(queens: Map[Int, String]): Unit = {
    selection = SelectionDisabled
  }

  override def queensInfoCommunicationsLateInit(): Any = {}
}
