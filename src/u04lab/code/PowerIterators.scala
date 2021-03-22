package u04lab.code

import Optionals._
import Optionals.Option._
import Lists._
import Streams._
import Streams.Stream._

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  /*
	 * @return a new iterator, to get all elements already produced, in reversed order.
	 * It does not alter the state of this iteration
	 */
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {
  /*
   * @return an iterator over the infinite sequence that begins with 'start', and applies 'successive' each time
   * recall that you compute the next of 'i' with instruction 'successive.apply(i)'
   */
  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  /*
   * @param list
   * @return an iterator over the element of the input list
   */
  def fromList[A](list: List[A])
  /*
   * @return an iterator over a sequence of random booleans, with length size
   * such a sequence should be produced "by need", since size could be very large
   */
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] =
    IteratorFromStream(Stream.iterate(start)(successive))

  private case class IteratorFromStream(stream: Stream[Int]) extends PowerIterator[Int] {
    private var pastList : List[Int] = List.Nil()
    private var i : Int = 1

    override def next(): Option[Int] = take(stream)(i) match {
      case Empty() => None()
      case s @ Stream.Cons(h, t) => {
        val res = getLastElem(s)
        pastList = List.append(pastList, List.Cons(res, List.Nil()))
        i = i + 1
        Some(res)
      }
    }

    private def getLastElem[A](stream: Stream[A]): A = List.reverse(Stream.toList(stream)) match {
      case List.Cons(head, _) => head
    }

    override def allSoFar(): List[Int] = pastList

    override def reversed(): PowerIterator[Int] = IteratorFromStream(toStream(List.reverse(pastList)))

    private def toStream[A](list: List[A]): Stream[A] = list match {
      case List.Cons(head, tail) => Stream.cons(head, toStream(tail))
      case List.Nil() => Stream.Empty()
    }

  }

  override def fromList[A](list: List[A]): Unit = ???

  override def randomBooleans(size: Int): PowerIterator[Boolean] = ???
}
