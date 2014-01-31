import sbt._
import Keys._
import sbtunidoc.Plugin._
import org.sbtidea.SbtIdeaPlugin._

object  Build extends sbt.Build {

  val ScalaVersion = "2.10.3"
  val Version = "0.0.1"

  import Resolvers._
  import Dependencies._

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization  := "feh.tec.agents",
    version       := Version,
    scalaVersion  := ScalaVersion,
//    scalacOptions ++= Seq("-explaintypes"),
//    scalacOptions ++= Seq("-deprecation"),
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-debug")
//     resolvers += Release.spray,
//     mainClass in Compile := Some("")
  )

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  object Resolvers{
    object Release{
      val sonatype = "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
      val spray = "spray" at "http://repo.spray.io/"
    }

    object Snapshot{
      val sonatype = "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
      val eulergui = "eulergui" at "http://eulergui.sourceforge.net/maven2"
    }

  }

  object Dependencies{
    lazy val akka = "com.typesafe.akka" %% "akka-actor" % "2.2.1"
    lazy val reflectApi = "org.scala-lang" % "scala-reflect" % ScalaVersion
    lazy val scalaSwing = "org.scala-lang" % "scala-swing" % ScalaVersion
    lazy val scalaCompiler = "org.scala-lang" % "scala-compiler" % ScalaVersion
    lazy val shapeless = "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1"

    object Apache{
      lazy val ioCommons = "commons-io" % "commons-io" % "2.4"
    }

    object spray{
      lazy val json = "io.spray" %%  "spray-json" % "1.2.5"
    }

    object Tests{
      lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
      lazy val specs2 = "org.specs2" %% "specs2" % "2.2.2" % "test"
    }

    lazy val svgSalamander = "com.kitfox.svg" % "svg-salamander" % "1.0"
    lazy val grappa = "att.grappa" % "grappa" % "1.2"

    object batik{
      lazy val BatikVersion = "1.6-1"

      lazy val swing = "batik" % "batik-swing" % BatikVersion
      lazy val svgGen = "batik" % "batik-svggen" % BatikVersion
      lazy val dom = "batik" % "batik-dom" % BatikVersion
      lazy val svgDom = "batik" % "batik-svg-dom" % BatikVersion
      lazy val util = "batik" % "batik-util" % BatikVersion
    }


    object feh{
      lazy val util = "feh" %% "util" % "1.0.1"

      object dsl{
        lazy val swing = "feh.dsl" %% "swing" % "1.0"
        lazy val graphviz = "feh.dsl" %% "graphviz" % "0.1"
      }
    }
  }

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings ++ unidocSettings ++ {
      name := "agent-comm"
    }
  ).settings(ideaExcludeFolders := ".idea" :: ".idea_modules" :: Nil)
   .aggregate(comm, svg, coloring)

  lazy val comm = Project(
    id = "comm",
    base = file("comm"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(akka, feh.util)
    )
  )

  lazy val svg = Project(
    id = "svg",
    base = file("svg"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
        svgSalamander,
        batik.svgGen,
        batik.swing,
        batik.util,
        batik.svgDom,
        feh.util,
        scalaSwing
      )
    )
  )

  lazy val coloring = Project(
    id = "coloring",
    base = file("coloring"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(feh.dsl.swing, feh.dsl.graphviz)
    )
  ) dependsOn(comm, svg)

}