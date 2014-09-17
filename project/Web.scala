import sbt._
import Keys._
import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

object Web{

  lazy val frontend = Seq(
    packTemplates <<= (fastOptJS in Compile, packageJSDependencies in Compile,
                        fullClasspath in Compile, streams, runner, baseDirectory, packDir) map {
      (compileJs, jsDep, cp, str, runner, base, dir) =>
        val params = Seq(base.toString, dir.toString, jsDep.getPath) ++ compileJs.cijsCode.map(_.path)
        runner.run("feh.tec.web.util.PackTemplates", cp.map(_.data), params, str.log)
    },
    packTemplatesOpt <<= (fullOptJS in Compile, packageJSDependencies in Compile,
      fullClasspath in Compile, streams, runner, baseDirectory, packDir) map {
      (compileJs, jsDep, cp, str, runner, base, dir) =>
        val params = Seq(base.toString, dir.toString, jsDep.getPath) ++ compileJs.ncjsCode.map(_.path)
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
  lazy val packTemplatesOpt = TaskKey   [Unit]("pack-templates-opt")
  lazy val cleanTemplates   = TaskKey   [Unit]("clean-templates")

  lazy val genNginxWSConf   = TaskKey   [Unit]("gen-nginx-ws")
}