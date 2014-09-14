package feh.tec.web.util

import feh.tec.web.NQueen
import feh.util.FileUtils._
import feh.util._
import scala.reflect._
import scala.scalajs.js.JSApp
import scala.xml.{Node, Xhtml, NodeSeq}


object GenTemplate{

}

class GenTemplate(val name: String, main: Class[JSApp]){
  def templateBody: NodeSeq = Nil
  def templateHead: NodeSeq = Nil

  def template: Node =
    <html>
      <head>
        <meta charset="UTF-8">
          <title> __set me!__ </title>
        </meta>
        <link rel="stylesheet" type="text/css" href={"../styles/" + name + ".css"}/>
        {templateHead}
      </head>
      <body>
        <!-- Include Scala.js compiled code -->
        <script type="text/javascript" src={jsPath("fastopt")}></script>
        <!-- Include JavaScript dependencies -->
        <script type="text/javascript" src={jsPath("jsdeps")}></script>
        <!-- Run App -->
        <script type="text/javascript"> {fixedName(main)}.main(); </script>
        {templateBody}
      </body>
    </html>

  protected def jsPath(nme: String): String = s"../target/scala-2.11/web-frontend-$nme.js"

  private def fixedName(main: Class[JSApp]) = main.getName match {
    case obj if obj endsWith "$" => obj.dropRight(1) + "()"
    case clazz => clazz
  }
}


object GenTemplates extends App{
  def clazz[T <: JSApp: ClassTag](t: => T) = classTag[T].runtimeClass.asInstanceOf[Class[JSApp]]

  val templates: List[GenTemplate] =
    new GenTemplate("n-queen", clazz(NQueen)) :: Nil
//    "n-queen" -> clazz(NQueen)

  val base = args(0)

  templates.zipMap(Xhtml toXhtml _.template).foreach{
    case (gen, xml) => file(base / "templates" / (gen.name + ".html")).createIfNotExists().get
      .withOutputStream(File.write.utf8(xml), append = false)
  }
}