/**
 * Type representing something that produces a ranked list of objects from a single query.
 * Also includes generic implementation of this concept.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.ml

import mlbigbook.data._
import mlbigbook.data.imut.BoundedPriorityQueue

import scala.annotation.tailrec
import scala.reflect.ClassTag

trait Ranker[T] extends (T => Traversable[(T, Double)])

case class RankerIn(rankFn: (OldVector, OldVector) => Double, limit: Int)

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

  def apply[T](d: Distance, limit: Int)(vecData: VectorDataIn[T]): Ranker[T] = {

    val (vectorizer, data) = vecData()

    (input: T) => {

      val vecInput = vectorizer(input)

      val f = (v: OldVector) => d(vecInput, v)

      takeTopK(f, limit, data)
        .map(x => (x._1, f(x._2)))
    }

  }

  /**
   * Evaluates to a Traversable containing the elements that have the largest associated values in the input. The
   * returned Traversable has at most limit items.
   */
  def takeTopK[T](f: OldVector => Double, limit: Int, elements: DataClass[(T, OldVector)]): Traversable[(T, OldVector)] = {

    val BoundPq = BoundedPriorityQueue.create[(T, OldVector)](OldVector.rankFnOrdering[T](f))(limit)

      @tailrec @inline def toTraversable(pq: BoundPq.T, existing: Seq[(T, OldVector)]): Traversable[(T, OldVector)] =
        BoundPq.takeMin(pq) match {

          case None =>
            existing.toTraversable

          case Some((min, rest)) =>
            toTraversable(rest, existing :+ min)
        }

    val resultingBpq =
      elements
        .aggregate(BoundPq.empty)(
          {
            case (pq, dataAndVector) =>
              BoundPq.insert(dataAndVector)(pq)
          },
          {
            case (pq1, pq2) =>
              BoundPq.merge(pq1, pq2)
          }
        )(ClassTag(BoundPq.empty.getClass))

    toTraversable(resultingBpq, Seq.empty[(T, OldVector)])
  }

  /**
   * Evaluates to a Traversable containing the elements that have the largest associated values in the input. The
   * returned Traversable has at most limit items.
   */
  def takeTopK[T, N](limit: Int, elements: DataClass[(T, N)])(
    implicit
    n: Numeric[N], c: ClassTag[N]
  ): Traversable[(T, N)] =
    elements
      .sortBy(_._2)(c, n.reverse)
      .take(limit)

  implicit class Fn[T](val f: T => Traversable[(T, Double)]) extends Ranker[T] {
    override def apply(x: T) = f(x)
  }

}
