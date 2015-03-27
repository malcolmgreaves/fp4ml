/**
 * Type representing something that produces a ranked list of objects from a single query.
 * Also includes generic implementation of this concept.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag

trait Ranker[T] extends (T => Traversable[(T, Double)])

case class RankerIn(rankFn: (Vector, Vector) => Double, limit: Int)

object Ranker {

  def apply[T](r: RankerIn)(vecData: VectorDataIn[T]): Ranker[T] = {

    val (vectorizer, data) = vecData()

    (input: T) => {
      val vecInput = vectorizer(input)
      takeTopK(
        r.limit,
        data.map({
          case (item, vecItem) => (item, r.rankFn(vecInput, vecItem))
        })
      )
    }
  }

  /**
   * Evaluates to a Traversable containing the elements that have the largest associated values in the input. The
   * returned Traversable has at most limit items.
   */
  def takeTopK[T, N](limit: Int, elements: DistData[(T, N)])(
    implicit n: Numeric[N], c: ClassTag[N]): Traversable[(T, N)] =
    elements
      .sortBy(_._2)(c, n.reverse)
      .take(limit)

  implicit class Fn[T](val f: T => Traversable[(T, Double)]) extends Ranker[T] {
    override def apply(x: T) = f(x)
  }

}
