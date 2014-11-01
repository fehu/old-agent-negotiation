package feh.tec.agents.light.spec.macros

import scala.reflect.macros.whitebox

object Configs {
  import feh.util.Path._

  def controller[C <: whitebox.Context](c: C) = {
    import c.universe._
    q"agents / controller.conf"
  }
}
