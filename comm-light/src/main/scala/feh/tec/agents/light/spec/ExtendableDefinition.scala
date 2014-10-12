package feh.tec.agents.light.spec

//import scala.reflect.runtime.{universe => ru}

trait ExtendableDefinition[Owner, Def] {
  def get(implicit owner: Owner): Def

  def extensionPoints: Map[String, ExtensionEntry[Any]]

  case class ExtensionEntry[+T](default: T, `override`: Option[T] = None){
    def get = `override` getOrElse default
  }

  protected def extension(name: String) = extensionPoints(name).get
}

protected class MonoDefinitionBase[Owner, Def](default: Owner => Def) extends ExtendableDefinition[Owner, Def]{
  def get(implicit owner: Owner): Def = DefExtension.get(owner)

  var DefExtension = ExtensionEntry(default)

  def extensionPoints: Map[String, ExtensionEntry[Any]] = Map("def" -> DefExtension)
}

class MonoDefinition[Owner, Def](default: Owner => Def) extends MonoDefinitionBase[Owner, Def](default)

class HiddenMonoDefinition[Owner, Def](default: Owner => Def) extends MonoDefinitionBase[Owner, Def](default)

object ExtendableDefinition{

  trait BeforeAndAfter[Def]{
    self: ExtendableDefinition[_, Def] =>

    var BeforeExtension = ExtensionEntry({})
    var AfterExtension = ExtensionEntry[Def => Def](identity)

    def extensionPoints: Map[String, ExtensionEntry[Any]] = Map("before" -> BeforeExtension, "after" -> AfterExtension)
  }

}