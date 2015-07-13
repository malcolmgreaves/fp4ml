/*
 * Contains methods for producing word count mappings from text data.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.wordcount

import mlbigbook.data.{ DistData, AddMap, Data }

import scala.collection.Map

abstract class NumericMap[@specialized N: Numeric] {

  type M[T] = Map[T, N]

  def empty[T]: M[T] = Map.empty[T, N]

  @inline final def increment[T](map: M[T], e: T): M[T] =
    increment(map, e, implicitly[Numeric[N]].one)

  final def increment[T](map: M[T], e: T, times: N): M[T] =
    if (map contains e)
      (map - e) + (e -> implicitly[Numeric[N]].plus(map(e), times))
    else
      map + (e -> times)

  final def increment[T](map: M[T], es: DistData[T]): M[T] =
    es.aggregate(map)(
      { case (m, e) => increment(m, e) },
      { case (m1, m2) => combine(m1, m2) }
    )

  /**
   * Combines two maps. If maps m1 and m2 both have key k, then the resulting
   * map will have m1(k) + m2(k) for the value of k.
   */
  final def combine[T](m1: M[T], m2: M[T]): M[T] = {
    val (smaller, larger) = if (m1.size < m2.size) (m1, m2) else (m2, m1)
    smaller.foldLeft(larger) {
      case (aggmap, (k, v)) =>
        aggmap.get(k) match {
          case Some(existing) =>
            (aggmap - k) + (k -> implicitly[Numeric[N]].plus(existing, v))
          case None =>
            aggmap + (k -> v)
        }
    }
  }

}

object NumericMap {

  implicit val int: NumericMap[Int] = new NumericMap[Int] {}
  implicit val long: NumericMap[Long] = new NumericMap[Long] {}
  implicit val float: NumericMap[Float] = new NumericMap[Float] {}
  implicit val double: NumericMap[Double] = new NumericMap[Double] {}

  type WordCount = Map[String, Long]
  val emptyWC: WordCount = long.empty[String]

}

/**
 * The Count object contains methods for computing the word count on a corpus, document,
 * and sentence level.
 */
object GenericCount {

  //
  //  abstract class Counter[T, @specialized N: Numeric] {
  //
  //    type Element = T
  //
  //    type Number = N
  //
  //    type State = Map[Element, Number]
  //
  //    val state: State
  //
  //    /*
  //
  //    final def count[T, N : Numeric](s: State, e: Element): State =
  //      count(s, e, 1)
  //
  //    final def count(s: State, e: Element, times: N): State =
  //      if(s contains e)
  //        (s - e) + (s -> (s(e) + times))
  //      else
  //        s + (e -> 1)
  //
  //    final def count(s: State, es: DistData[T]): State = {
  //
  //    }
  //
  //    final def count(s: State, es: DistData[T]): State = {
  //      es
  //        .aggregate(s)
  //    }
  //
  //     */
  //
  //  }

}