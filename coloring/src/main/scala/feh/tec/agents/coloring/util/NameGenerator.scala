package feh.tec.agents.coloring.util

class NameGenerator(forbidden: Set[String]){
  protected var next = "a"

  private def incr(str: String) = {
    def inner(str: List[Char]): List[Char] = str match{
      case 'Z' :: tail => 'Z' :: inner(tail)
      case 'z' :: tail => 'A' :: tail
      case az :: tail if 'a' <= az && az < 'z' || 'A' <= az && az < 'Z' => (az.toInt + 1).toChar :: tail
    }

    if(str.distinct == "Z") "a" * (str.length + 1)
    else inner(str.toList.reverse).reverse.mkString
  }

  def nextName: Name = {
    val r = next
    next = incr(next)
    Name(r)
  }
}

object Name{
  implicit def nameToStringWrapper(n: Name) = n.name
}
case class Name(name: String){
  override def toString = name
}