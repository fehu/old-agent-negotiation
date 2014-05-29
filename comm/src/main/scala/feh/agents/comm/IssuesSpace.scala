package feh.agents.comm

trait IssuesSpace {
  def issues: Seq[Issue]
  def dim = issues.length

  type Point // tuple of issue#Value
}

trait Issue{
  def name: String

  type Value
//  def valueClass: Class[_]
}

//trait IssuesSpaceTransformation[-From, +To]{
//
//}