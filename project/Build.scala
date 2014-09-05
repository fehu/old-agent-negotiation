import sbt._
import Keys._
import sbtunidoc.Plugin._
import org.sbtidea.SbtIdeaPlugin._

object  Build extends sbt.Build {

  val ScalaVersion = "2.11.2"
  val Version = "0.1"

  import Resolvers._
  import Dependencies._

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization  := "feh.agents",
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
    lazy val akka = "com.typesafe.akka" %% "akka-actor" % "2.3.3"
    lazy val shapeless = "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1"

    object scala{
      lazy val compiler = "org.scala-lang" % "scala-compiler" % ScalaVersion
      lazy val swing = "org.scala-lang" % "scala-swing" % ScalaVersion
      lazy val reflectApi = "org.scala-lang" % "scala-reflect" % ScalaVersion
    }

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

    object jung{
      lazy val JungVersion = "2.0.1"

      lazy val jung2 = "net.sf.jung" % "jung2" % JungVersion
      lazy val api = "net.sf.jung" % "jung-api" % JungVersion
      lazy val graph = "net.sf.jung" % "jung-graph-impl" % JungVersion
      lazy val visualization = "net.sf.jung" % "jung-visualization" % JungVersion
      lazy val algorithms = "net.sf.jung" % "jung-algorithms" % JungVersion

      def all = jung2 :: api :: graph :: visualization :: algorithms :: Nil
    }


    object feh{
      lazy val util = "feh.util" %% "util" % "1.0.4"

      object utils{
        lazy val compiler = "feh.util" %% "scala-compiler-utils" % "0.1"
      }

      object dsl{
        lazy val swing = "feh.dsl" %% "swing" % "1.2"
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
   .aggregate(comm, oldcomm, coloring, misc)

  lazy val comm = Project(
    id = "comm",
    base = file("comm"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(akka, feh.util, scala.reflectApi)
    )
  )

  lazy val misc = Project(
    id = "misc",
    base = file("misc"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
        feh.dsl.swing
      )
    )
  ) dependsOn comm


  lazy val oldcomm = Project(
    id = "oldcomm",
    base = file("oldcomm"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(akka, feh.util)
    )
  )

  lazy val coloring = Project(
    id = "coloring",
    base = file("coloring"),
    settings = buildSettings ++ Seq(
      resolvers += Snapshot.sonatype,
      libraryDependencies ++= Seq(
        feh.util,
        feh.dsl.swing,
        feh.utils.compiler
      ) ++ jung.all
    )
  ) dependsOn oldcomm

}
