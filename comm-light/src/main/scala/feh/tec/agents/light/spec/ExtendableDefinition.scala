package feh.tec.agents.light.spec

trait ExtendableDefinition[Owner, Def] {
  def get(implicit owner: Owner): Def

  def extensionPoints: Map[String, ExtensionEntry[Any]]

  case class ExtensionEntry[+T](default: T, `override`: Option[T] = None){
    def get = `override` getOrElse default
  }

  protected def extension(name: String) = extensionPoints(name).get
}

class MonoDefinition[Owner, Def](default: Owner => Def) extends ExtendableDefinition[Owner, Def]{
  def get(implicit owner: Owner): Def = DefExtension.get(owner)

  var DefExtension = ExtensionEntry(default)

  def extensionPoints: Map[String, ExtensionEntry[Any]] = Map("def" -> DefExtension)
}

object ExtendableDefinition{

  trait BeforeAndAfter[Def]{
    self: ExtendableDefinition[_, Def] =>

    var BeforeExtension = ExtensionEntry({})
    var AfterExtension = ExtensionEntry[Def => Def](identity)

    def extensionPoints: Map[String, ExtensionEntry[Any]] = Map("before" -> BeforeExtension, "after" -> AfterExtension)
  }

}