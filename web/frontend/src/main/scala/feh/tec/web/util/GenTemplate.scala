package feh.tec.web.util

import java.io.File
import feh.util.{AbsolutePath, Path, PathSelector}
import feh.util.file._
import scala.reflect._
import scala.scalajs.js.JSApp
import scala.xml.{Node, NodeSeq}


object GenTemplate{
  def classOf[T <: JSApp: ClassTag](t: => T) = classTag[T].runtimeClass.asInstanceOf[Class[JSApp]]
}

class GenTemplate(val name: String, main: Class[JSApp]){
  def templateBody: NodeSeq = Nil
  def templateHead: NodeSeq = Nil
  def css: PathSelector = PathSelector.empty
  def includeStyleResources = PathSelector.empty

  def template(cssPaths: Seq[Path], jsPaths: Seq[Path]): Node =
    <html>
      <head>
        <meta charset="UTF-8">
          <title> __set me!__ </title>
        </meta>
        {cssPaths map genCssLink}
        {templateHead}
      </head>
      <body>
        <!-- Include Scala.js compiled code and dependencies -->
        {jsPaths map genJsImport}
        <!-- Run App -->
        <script type="text/javascript"> {fixedName(main)}.main(); </script>
        {templateBody}
      </body>
    </html>

  def cssFiles(basedir: AbsolutePath) = (basedir / "styles" / css ).get[(Path, File)]

  protected def genCssLink(path: Path) = <link rel="stylesheet" type="text/css" href={path.toString}/>
  protected def genJsImport(path: Path) = <script type="text/javascript" src={path.toString}></script>

  private def fixedName(main: Class[JSApp]) = main.getName match {
    case obj if obj endsWith "$" => obj.dropRight(1) + "()"
    case clazz => clazz
  }
}

