/*
 * A nearest neighbors implementation.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.ml

import mlbigbook.data._

case class NearNeighIn(dist: Distance, neighborhoodSize: Int)

object NnRanker {

  /**
   * Produces a Nearest Neighbors ranker.
   *
   * Uses Ranker.apply underneath as the ranking algorithm.
   */
  def apply[T](n: NearNeighIn)(vdata: VectorDataIn[T]): Ranker[T] =
    Ranker(n)(vdata)

  implicit def nnIn2RankerIn(n: NearNeighIn): RankerIn =
    RankerIn(n.dist.apply, n.neighborhoodSize)
}