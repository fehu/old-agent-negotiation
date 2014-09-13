import sbt._
import Keys._

object Web{

  lazy val settings = Seq(
    genTemplates <<= (state, fullClasspath in Compile, streams, runner, baseDirectory) map { (s, cp, str, runner, base) =>
      Run.run("feh.tec.web.util.GenTemplates", cp.map(_.data), Seq(base.toString), str.log)(runner)
    }
  )


  lazy val genTemplates = TaskKey[Unit]("gen-templates")
}