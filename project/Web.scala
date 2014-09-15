import sbt._
import Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

object Web{

  lazy val frontend = Seq(
    packTemplates <<= (fullOptJS in Compile, packageJSDependencies in Compile,
                        fullClasspath in Compile, streams, runner, baseDirectory, packDir) map {
      (fullOptJS, jsDep, cp, str, runner, base, dir) =>
        val params = Seq(base.toString, dir.toString, jsDep.getPath) ++ fullOptJS.ncjsCode.map(_.path)
        runner.run("feh.tec.web.util.PackTemplates", cp.map(_.data), params, str.log)
    },
    cleanTemplates := IO.delete(packDir.value)
  )

  lazy val backend = Seq(
    genNginxWSConf <<=  (fullClasspath in Compile, streams, runner, baseDirectory) map { (cp, str, runner, base) =>
      Run.run("feh.tec.web.util.GenNginxConfigs", cp.map(_.data), Seq(base.toString), str.log)(runner)
    }
  )


  lazy val packDir          = SettingKey[File]("web-pack-dir")
  lazy val packTemplates    = TaskKey   [Unit]("pack-templates")
  lazy val cleanTemplates   = TaskKey   [Unit]("clean-templates")

  lazy val genNginxWSConf   = TaskKey   [Unit]("gen-nginx-ws")
}