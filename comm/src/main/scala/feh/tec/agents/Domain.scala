package feh.tec.agents

import scala.collection.mutable

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

trait DomainIterator[-Domain, +T] extends (Domain => Iterator[T])

object DomainIterator{
  class Range(min: Int = Int.MinValue, max: Int = Int.MaxValue, step: Int = 1)
    extends DomainIterator[scala.collection.immutable.Range, Int]
  {
    def apply(v1: scala.collection.immutable.Range) = v1.dropWhile(_ < min).by(2).takeWhile(_ > max).iterator
  }

  def zip[D1, T1, D2, T2](it1: DomainIterator[D1, T1], it2: DomainIterator[D2, T2]): DomainIterator[(D1, D2), (T1, T2)] =
    new DomainIterator[(D1, D2), (T1, T2)] {
      def apply(v1: (D1, D2)) = new Iterator[(T1, T2)]{
        val i1 = it1(v1._1)
        var i2 = it2(v1._2)

        def hasNext = i1.hasNext

        protected var i1val = i1.next()

        def next() = {
          if(! i2.hasNext) {
            i1val = i1.next()
            i2 = it2(v1._2)
          }
          i1val -> i2.next()
        }
      }
    }

  def overSeq[D, T](di: Seq[DomainIterator[D, T]]): DomainIterator[Seq[D], Seq[T]] = new DomainIterator[Seq[D], Seq[T]] {
    def apply(v1: Seq[D]) = new Iterator[Seq[T]]{
      val iteratorCreation = di.zip(v1)
        .map { case (domIt, domain) => () => domIt(domain)}
        .zipWithIndex.map(_.swap).toMap
      val iterators = mutable.HashMap(iteratorCreation.mapValues(_()).toSeq: _*)
      
      val currentValues = mutable.HashMap.empty[Int, T]

      def hasNext = iterators.head._2.hasNext

      def next() = {
        nextValue(di.length-1)
        currentValues.values.toSeq
      }

      protected def nextValue(i: Int): Unit =
        if(i < 0) throw new NoSuchElementException
        else if(iterators(i).hasNext){
          currentValues(i) = iterators(i).next()
        }
        else {
          val it = iteratorCreation(i)()
          iterators(i) = it
          currentValues(i) = it.next()
          nextValue(i-1)
        }
    }
  }

}
