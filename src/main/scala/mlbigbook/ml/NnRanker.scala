/*
 * A nearest neighbors implementation.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.ml

import mlbigbook.data._

object NnRanker {

  /**
   * Produces a Nearest Neighbors ranker.
   *
   * Uses Ranker.apply underneath as the ranking algorithm.
   */
  def apply[T](
    dist: Distance,
    kNeighborhoodSize: Int,
    mkVec: VectorizerMaker[T],
    data: DistData[T]): Ranker[T] =

    Ranker(dist.apply, kNeighborhoodSize, mkVec, data)
}