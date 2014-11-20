package feh.tec.web.gen

import feh.tec.web.util.GenTemplate


object TemplatesToGenerate {
  val templates: List[GenTemplate] = new NQueenTemplate :: new NQueenLiteTemplate :: Nil
}
