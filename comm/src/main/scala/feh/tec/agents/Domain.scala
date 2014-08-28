package feh.tec.agents

import akka.actor.ActorRef

abstract class Var(val name: String, test: Any => Boolean){
  type Tpe
  type Domain
  def domain: Domain

  def cast(a: Any): Option[Tpe] = if(test(a)) Some(a.asInstanceOf[Tpe]) else None

  override def toString = name
}

object Domain{
  trait Small[T]{
    self: Var =>

    type Tpe = T
    type Domain = Set[T]
  }

  trait Range{
    self: Var =>

    type Tpe = Int
    type Domain = scala.collection.immutable.Range
  }

}

trait DomainIterator[Domain, T] extends (Domain => Iterator[T])

object DomainIterator{
  class Range(min: Int = Int.MinValue, max: Int = Int.MaxValue, step: Int = 1)
    extends DomainIterator[scala.collection.immutable.Range, Int]
  {
    def apply(v1: scala.collection.immutable.Range) = v1.dropWhile(_ < min).by(2).takeWhile(_ > max).iterator
  }
}

case class AgentRef(id: Agent.Id, ref: ActorRef)

