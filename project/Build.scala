import sbt._
import Keys._
import sbtunidoc.Plugin._
import org.sbtidea.SbtIdeaPlugin._
import scala.scalajs.sbtplugin._
import ScalaJSPlugin._

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
//    scalacOptions ++= Seq("-Ydebug"),
//    scalacOptions ++= Seq("-Xlog-free-terms"),
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-diagrams-max-classes", "50", "-diagrams-max-implicits", "20")
//     resolvers += Release.spray,
//     mainClass in Compile := Some("")
  )

  lazy val testSettings = TestSettings.get ++ Seq(
    TestSettings.copyTestReportsDir <<= baseDirectory(base => Some(base / "test-reports")),
    TestSettings.autoAddReportsToGit := true
  )

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  object Resolvers{
    object Release{
      lazy val sonatype = "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
      lazy val spray = "spray" at "http://repo.spray.io/"
    }

    object Snapshot{
      lazy val sonatype = "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
      lazy val eulergui = "eulergui" at "http://eulergui.sourceforge.net/maven2"
      lazy val spray = "spray nightlies repo" at "http://nightlies.spray.io"
    }

  }

  object Dependencies{
    lazy val akka = "com.typesafe.akka" %% "akka-actor" % "2.3.3"
    lazy val shapeless = "com.chuusai" % "shapeless_2.10.2" % "2.0.0-M1"

    object scala{
      lazy val compiler = "org.scala-lang" % "scala-compiler" % ScalaVersion
      lazy val swing = "org.scala-lang" % "scala-swing" % ScalaVersion
      lazy val reflectApi = "org.scala-lang" % "scala-reflect" % ScalaVersion
      lazy val libAll = "org.scala-lang" % "scala-library-all" % ScalaVersion // 2.11.x
    }

    object typesafe{
      lazy val config = "com.typesafe" % "config" % "1.2.1"
    }

    object Apache{
      lazy val ioCommons = "commons-io" % "commons-io" % "2.4"
    }

    object spray{
      lazy val json = "io.spray" %%  "spray-json" % "1.2.6"
      lazy val can = "io.spray" %% "spray-can" % "1.3.1"
      lazy val websocket= "com.wandoulabs.akka" %% "spray-websocket" % "0.1.3"
    }

    object Tests{
      lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
      lazy val specs2 = "org.specs2" %% "specs2" % "2.4.2" % "test"
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
      lazy val util = "feh.util" %% "util" % "1.0.5"

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
   .aggregate(comm, oldcomm, coloring, misc, webFrontend, webBackend, negDSL)

  lazy val comm = Project(
    id = "comm",
    base = file("comm"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(akka, feh.util, scala.reflectApi)
    )
  ) dependsOn negDSL

  lazy val negDSL: Project = Project(
    "negotiation-dsl",
    file("negotiation-dsl"),
    settings = buildSettings ++ testSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
    )
  )

  lazy val misc = Project(
    id = "misc",
    base = file("misc"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq()
    )
  ) dependsOn (comm, webBackend)


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

  lazy val webCommon = Project("web-common", file("web/common"),
    settings = buildSettings ++ Seq(
      libraryDependencies += typesafe.config
    )
  )

  lazy val webFrontend = Project(// libraryDependencies in build.sbt
    id = "web-frontend",
    base = file("web/frontend"),
    settings = buildSettings ++ Web.frontend ++ Seq(
      Web.packDir := file("web/packed"),
      libraryDependencies ++= Seq(feh.util, scala.libAll),
      unmanagedSourceDirectories in Compile <+= (sourceDirectory in webCommon)
    )
  ) dependsOn webCommon

  lazy val webBackend = Project(
    id = "web-backend",
    base = file("web/backend"),
    settings = buildSettings ++ Web.backend ++ Seq(
      resolvers ++= Seq(Snapshot.spray, Release.spray),
      libraryDependencies ++= Seq(feh.util, spray.websocket, spray.json)
    )
  ) dependsOn webCommon

}
