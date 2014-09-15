package feh.tec.web.util

import scala.xml.Xhtml
import feh.util.file._
import feh.util._

/** Generates templates registered in `TemplatesToGenerate`, copy them and css associated to target */
object PackTemplates extends App {
  val basedir = args(0)
  val target = args(1)

  val js = args.toList.drop(2).map(_.p)

  TemplatesToGenerate.templates foreach {
    gen =>

      target.toFile.mkdir()
      val cssFiles = gen.cssFiles(basedir.toAbsolute).map{case (p, f) => p.drop(basedir / "styles") -> f}
      val xml = Xhtml toXhtml gen.template(cssFiles.map(_._1), js.map(_.name.p))

      File(target / (gen.name + ".html"))
        .withOutputStream(File.write.utf8(xml), append = false)

      cssFiles map {
        case (path, file) => file.cp(target / path, overwrite = true)
      }

      js foreach{
        path => path.file.cp(target / path.name, overwrite = true)
      }
  }
}