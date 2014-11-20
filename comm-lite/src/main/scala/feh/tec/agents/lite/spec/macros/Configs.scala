package feh.tec.agents.lite.spec.macros

import scala.reflect.macros.whitebox

object Configs {
  def controller[C <: whitebox.Context](c: C) = {
    import c.universe._
    q"""
      import feh.util.Path._
      `.` / "agents" / "controller.conf"
    """
  }
}
