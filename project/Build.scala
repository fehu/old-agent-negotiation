import org.sbtidea.SbtIdeaPlugin._
import sbt.Keys._
import sbt._
import sbtunidoc.Plugin.UnidocKeys._
import sbtunidoc.Plugin._

object  Build extends sbt.Build {

  val ScalaVersion = "2.11.4"
  val Version = "0.3-SNAPSHOT"

  import Build.Dependencies._
  import Build.Resolvers._

  val buildSettings = Defaults.coreDefaultSettings ++ Seq (
    organization  := "feh.agents",
    version       := Version,
    scalaVersion  := ScalaVersion,
//    javaOptions   += "-Xmx4000M -XX:MaxPermSize=1024 -Xss1M",
//    scalacOptions ++= Seq("-explaintypes"),
//    scalacOptions ++= Seq("-deprecation"),
//    scalacOptions ++= Seq("-Ydebug"),
//    scalacOptions ++= Seq("-Xlog-free-terms"),
    scalacOptions ++= Seq("-Ymacro-debug-lite"),
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
    def AkkaVersion = "2.3.3"
    lazy val akka = "com.typesafe.akka" %% "akka-actor" % AkkaVersion
    lazy val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % AkkaVersion
    lazy val slf4j = "org.slf4j" % "slf4j-log4j12" % "1.7.7"

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
      lazy val util = ProjectRef( uri("git://github.com/fehu/util.git"), "util")

//      object utils{
//        lazy val compiler = ProjectRef( uri("git://github.com/fehu/util.git"), "scala-compiler-utils")
//      }
    }

    object js{
      lazy val jquery = "org.webjars" % "jquery" % "2.1.1"
      lazy val bootstrap = "org.webjars" % "bootstrap" % "3.2.0"
    }

    lazy val GitDependencies = feh.util :: Nil //feh.utils.compiler
  }

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings ++ unidocSettings ++ Seq(
      name := "agent-comm",
      unidocScopeFilter in (ScalaUnidoc, unidoc) := {
        (unidocScopeFilter in (ScalaUnidoc, unidoc)).value -- ScopeFilter( inProjects(webFrontend), inConfigurations(Compile) )
      }
    )
  ).settings(ideaExcludeFolders := ".idea" :: ".idea_modules" :: Nil)
   .aggregate(GitDependencies: _*)
   .aggregate(commLite, apps, webFrontend, webBackend)

  lazy val commLite = Project("comm-lite", file("comm-lite"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(akka, scala.reflectApi),
      initialCommands += "import feh.tec.agents.lite._"
    )
  ) dependsOn feh.util


  lazy val apps = Project(
    id = "apps",
    base = file("apps"),
    settings = buildSettings ++ testSettings ++ Seq(
      libraryDependencies ++= Seq(akkaSlf4j, slf4j),
      initialCommands += "import feh.tec.agents.lite._"
    )
  ) dependsOn (webBackend, commLite)

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
      libraryDependencies ++= Seq(scala.libAll),
      unmanagedSourceDirectories in Compile <+= (sourceDirectory in webCommon)

    )
  ) dependsOn (feh.util, webCommon)

  lazy val webBackend = Project(
    id = "web-backend",
    base = file("web/backend"),
    settings = buildSettings ++ Web.backend ++ Seq(
      resolvers ++= Seq(Snapshot.spray, Release.spray),
      libraryDependencies ++= Seq(spray.websocket, spray.json)
    )
  ) dependsOn (feh.util, webCommon)
}
