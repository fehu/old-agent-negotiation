package feh.tec.web.util

import feh.tec.web.NQueen
import feh.util.FileUtils._
import feh.util._
import scala.reflect._
import scala.scalajs.js.JSApp
import scala.xml.{Node, Xhtml, NodeSeq}


object GenTemplate{
  def jsPath(name: String): String = s"../target/scala-2.11/web-frontend-$name.js"

  def template(name: String, main: Class[JSApp]): Node =
<html>
  <head>
    <meta charset="UTF-8">
      <title> __set me!__ </title>
    </meta>
    <link rel="stylesheet" type="text/css" href={"../styles/" + name + ".css"}/>
  </head>
  <body>
    <!-- Include Scala.js compiled code -->
    <script type="text/javascript" src={jsPath("fastopt")}></script>
    <!-- Include JavaScript dependencies -->
    <script type="text/javascript" src={jsPath("jsdeps")}></script>
    <!-- Run App -->
    <script type="text/javascript"> {fixedName(main)}.main(); </script>
  </body>
</html>

  private def fixedName(main: Class[JSApp]) = main.getName match {
    case obj if obj endsWith "$" => obj.dropRight(1) + "()"
    case clazz => clazz
  }
}


object GenTemplates extends App{
  def clazz[T <: JSApp: ClassTag](t: => T) = classTag[T].runtimeClass.asInstanceOf[Class[JSApp]]

  val templates: Map[String, Class[JSApp]] = Map(
    "n-queen" -> clazz(NQueen)
  )

  val base = args(0)

  templates.zipMap((GenTemplate.template _).tupled andThen Xhtml.toXhtml).foreach{
    case (name, xml) => file(base / "templates" / (name + ".html")).createIfNotExists().get
      .withOutputStream(File.write.utf8(xml), append = false)
  }
}