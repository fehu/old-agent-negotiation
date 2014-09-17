import ScalaJSKeys._

import Dependencies._

scalaJSSettings

libraryDependencies ++= Seq(
  "org.scala-lang.modules.scalajs" %%% "scalajs-jquery" % "0.6",
  "com.scalatags" %%% "scalatags" % "0.4.0"
)

skip in packageJSDependencies := false

jsDependencies ++= Seq(
  js.jquery      / "jquery.js",
  js.bootstrap   / "bootstrap.js",
  ProvidedJS / "jquery.tablesorter.js"            dependsOn "jquery.js",
  ProvidedJS / "jquery.tablesorter.widgets.js"    dependsOn "jquery.tablesorter.js",
  ProvidedJS / "jquery.tablesorter.pager.js"      dependsOn "jquery.tablesorter.widgets.js"
)