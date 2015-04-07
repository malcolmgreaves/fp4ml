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

case class RankerIn(rankFn: (Vector, Vector) => Double, limit: Int)

object Ranker {

  def apply[T](r: RankerIn)(vecData: VectorDataIn[T]): Ranker[T] = {

    val (vectorizer, data) = vecData()

    (input: T) => {

      val vecInput = vectorizer(input)

      val f = (v:Vector) => r.rankFn(vecInput, v)

      val top = takeTopK(f, r.limit, data)

      top.map(x => (x._1, f(x._2)))
    }
//      {
//      val vecInput = vectorizer(input)
//      OLD_takeTopK(
//        r,
//        data.map({
//          case (item, vecItem) => (item, r.rankFn(vecInput, vecItem))
//        })
//      )
//    }
  }

  /**
   * Evaluates to a Traversable containing the elements that have the largest associated values in the input. The
   * returned Traversable has at most limit items.
   */

  def takeTopK[T](f:Vector => Double, limit:Int, elements: DistData[(T, Vector)]): Traversable[(T, Vector)] = {

    val BoundPq = BoundedPriorityQueue.create[(T,Vector)](Vector.rankFnOrdering[T](f))(limit)

    @tailrec @inline def toTraversable(pq:BoundPq.T, existing:Seq[(T,Vector)]):Traversable[(T,Vector)] =
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

    toTraversable(resultingBpq, Seq.empty[(T,Vector)])
  }

  @deprecated
  def OLD_takeTopK[T, N](limit: Int, elements: DistData[(T, N)])(
    implicit n: Numeric[N], c: ClassTag[N]): Traversable[(T, N)] =
    elements
      .sortBy(_._2)(c, n.reverse)
      .take(limit)

  implicit class Fn[T](val f: T => Traversable[(T, Double)]) extends Ranker[T] {
    override def apply(x: T) = f(x)
  }

}
