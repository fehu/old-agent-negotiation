package feh.tec.web

object Utils {
  implicit class $$Wrapper[T](t: T){
    def $$(f: T => Unit): T = {
      f(t)
      t
    }
    def $(f0: => Unit): T = {
      f0
      t
    }
  }
}
