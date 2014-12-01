package feh.tec.agents.lite

import scala.collection.{IterableLike, mutable}
import feh.util._

abstract class Var(val name: String, testType: Any => Boolean){
  type Tpe
  type Domain
  def domain: Domain

  def cast(a: Any): Option[Tpe] = if(testType(a)) Some(a.asInstanceOf[Tpe]) else None

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

trait DomainIteratorBuilder[-Domain, +T] extends (Domain => Iterator[T])

trait StraightForwardDomainIteratorBuilder[-Domain, +T] extends DomainIteratorBuilder[Domain, T]
trait RandomOrderDomainIteratorBuilder[-Domain, +T] extends DomainIteratorBuilder[Domain, T]

trait DomainSeqIteratorBuilder[Domain, T] extends DomainIteratorBuilder[Seq[Domain], Seq[T]]{
  type WithUnderlying = Iterator[Seq[T]] {
    def iterators: mutable.HashMap[Int, scala.Iterator[T]]
  }

  override def apply(v1: Seq[Domain]): WithUnderlying
}

object DomainIteratorBuilder{

  class Range(min: Int = Int.MinValue, max: Int = Int.MaxValue, step: Int = 1)
    extends StraightForwardDomainIteratorBuilder[scala.collection.immutable.Range, Int]
  {
    def apply(v1: scala.collection.immutable.Range) = v1.dropWhile(_ < min).by(step).takeWhile(_ < max).iterator
  }
  
  class Generic[T] extends StraightForwardDomainIteratorBuilder[IterableLike[T, _], T]{
    def apply(v1: IterableLike[T, _]) = new Iterator[T]{
      val it = v1.iterator

      def hasNext = it.hasNext
      def next() = it.next()
    }
  }

  def zip[D1, T1, D2, T2](it1: DomainIteratorBuilder[D1, T1], it2: DomainIteratorBuilder[D2, T2]) =
    new StraightForwardDomainIteratorBuilder[(D1, D2), (T1, T2)] {
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

  def constant[D, T](value: T, size: Int): DomainIteratorBuilder[D, T] = new DomainIteratorBuilder[D, T]{
    def apply(v1: D): Iterator[T] = Stream.fill(size)(value).iterator
  }

  def overSeq[D, T](di: Seq[DomainIteratorBuilder[D, T]]): StraightForwardDomainIteratorBuilder[Seq[D], Seq[T]] with DomainSeqIteratorBuilder[D, T] =
    new StraightForwardDomainIteratorBuilder[Seq[D], Seq[T]] with DomainSeqIteratorBuilder[D, T]{
      def apply(v1: Seq[D]): WithUnderlying = new Iterator[Seq[T]]{
        val iteratorCreation = di.zip(v1)
          .map { case (domIt, domain) => () => domIt(domain)}
          .zipWithIndex.map(_.swap).toMap
        val iterators = mutable.HashMap(iteratorCreation.mapValues(_()).toSeq: _*)

        val currentValues = mutable.HashMap.empty[Int, T]

        def hasNext = iterators.exists(_._2.hasNext)

        var isFirstTime = true

        def next() = {
          if(isFirstTime){
            for(i <- 0 until di.length) nextValue(i)
            isFirstTime = false
          }
          else nextValue(0)

          currentValues.toList.sortBy(_._1).map(_._2)
        }

        protected def nextValue(i: Int): Unit =
          if(i == di.length) {} //throw new NoSuchElementException
          else if(iterators(i).hasNext){
            currentValues(i) = iterators(i).next()
          }
          else {
            val it = iteratorCreation(i)()
            iterators(i) = it
            currentValues(i) = it.next()
            nextValue(i+1)
          }
      }
  }

  object Random{
    def apply[D, T](dit: DomainIteratorBuilder[D, T]): RandomOrderDomainIteratorBuilder[D, T] = dit match{
      case rand: RandomOrderDomainIteratorBuilder[_, _] => ???
      case _ => new RandomOrderDomainIteratorBuilder[D, T] {
        def apply(v1: D) = {
          val underlying = dit(v1)
          underlying.toSeq.randomOrder().iterator
        }
      }
    }

    def apply[D, T](applyRandom: Int*)(dit: DomainSeqIteratorBuilder[D, T]): DomainSeqIteratorBuilder[D, T] with RandomOrderDomainIteratorBuilder[Seq[D], Seq[T]] =
      dit match {
        case rand: RandomOrderDomainIteratorBuilder[_, _] => ???
        case _ => new DomainSeqIteratorBuilder[D, T] with RandomOrderDomainIteratorBuilder[Seq[D], Seq[T]] {
          def apply(v1: Seq[D]) = {
            val underlying = dit(v1)
            underlying.iterators.withFilter(applyRandom contains _._1) foreach{
              case (i, it) => underlying.iterators(i) = it.toSeq.randomOrder().iterator
            }
            underlying
          }
        }
      }
  }
}
