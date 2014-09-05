import ScalaJSKeys._

scalaJSSettings

libraryDependencies += "org.scala-lang.modules.scalajs" %%% "scalajs-jquery" % "0.6"

skip in packageJSDependencies := false

