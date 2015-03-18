/**
 * Type representing something that produces a ranked list of objects from a single query.
 * Also includes generic implementation of this concept.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.ml

import mlbigbook.data.{ VectorizerMaker, Vector, DistData, Vectorizer }

import scala.reflect.ClassTag

trait Ranker[T] extends (T => Traversable[(Double, T)])

object Ranker {

  type RankingFn = (Vector, Vector) => Double

  def apply[T](rank: RankingFn, docLimit: Int, mkVec: VectorizerMaker[T], data: DistData[T]): Ranker[T] = {

    val vectorizer = mkVec(data)
    val vectorizedDocuments = data.map(d => (d, vectorizer(d)))

    (input: T) => {
      val vecInputDoc = vectorizer(input)
      takeTopK(
        docLimit,
        vectorizedDocuments.map({ case (doc, vec) => (rank(vec, vecInputDoc), doc) })
      )
    }
  }

  /**
   * Evaluates to a Traversable containing the elements that have the largest associated values in the input. The
   * returned Traversable has at most limit items.
   */
  def takeTopK[T, N](limit: Int, elements: DistData[(N, T)])(
    implicit n: Numeric[N], c: ClassTag[N]): Traversable[(N, T)] =
    elements
      .sortBy(_._1)(c, n.reverse)
      .take(limit)

  implicit def fn2ranker[T](f: T => Traversable[(Double, T)]): Ranker[T] =
    new Ranker[T] {
      override def apply(x: T): Traversable[(Double, T)] = f(x)
    }

}
