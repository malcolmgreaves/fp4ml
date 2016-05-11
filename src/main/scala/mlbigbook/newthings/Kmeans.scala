package mlbigbook.newthings

import fif.Data

import scala.annotation.tailrec
import scala.language.{ higherKinds, reflectiveCalls }
import scala.reflect.ClassTag

trait Kmeans extends Clustering {

  /** Creates a pseudo-random number generator for the type N. */
  val mkRandomNumGen: () => RandoMut[N]

  // ClassTag evidence necessary for MathVectorOps, Data abstractions
  protected[Kmeans] implicit val ctN: ClassTag[N]

  // Brings in the Data type class operations as methods "accessible" using
  // familiar object dot notation.
  // i.e. `data.map` instead of `implicitly[Data[D]].map(data)`
  import Data.ops._

  override final def cluster[D[_]: Data](
    conf:  ClusteringConf,
    dist:  Distance,
    toVec: Vectorizer
  )(data: D[Item]): Seq[Center] =
    cluster_h(
      conf,
      dist,
      toVec,
      0,
      data map { toVec.vectorize },
      initialize(conf.nClusters, toVec.nDimensions)
    )

  final def initialize(
    nClusters:   Int,
    nDimensions: Int
  ): Seq[Center] = {
    val r = mkRandomNumGen()
    (0 until nClusters)
      .map { id =>
        Center(
          id = id.toString,
          mean = vops.map(vops.ones(nDimensions)) {
            one => vops.n.times(one, r.next())
          }
        )
      }
      .toSeq
  }

  @tailrec
  @inline
  private[this] final def cluster_h[D[_]: Data](
    conf:        ClusteringConf,
    dist:        Distance,
    toVec:       Vectorizer,
    currIter:    Int,
    data:        D[V[N]],
    currCenters: Seq[Center]
  ): Seq[Center] =

    if (currIter >= conf.maxIterations)
      currCenters

    else {
      val updatedCenters = updateCenters(dist, toVec, currCenters, data)

      val sumSquaredChangeInMeansBetweenIters =
        currCenters.zip(updatedCenters)
          .foldLeft(0.0) {
            case (accum, (existing, updated)) =>
              val d = math.abs(
                implicitly[Numeric[N]].toDouble(
                  dist(existing.mean, updated.mean)
                )
              )
              accum + d
          }

      if (sumSquaredChangeInMeansBetweenIters < conf.tolerance)
        updatedCenters

      else
        cluster_h(
          conf,
          dist,
          toVec,
          currIter + 1,
          data,
          updatedCenters
        )
    }

  def updateCenters[D[_]: Data](
    dist:    Distance,
    toVec:   Vectorizer,
    centers: Seq[Center],
    data:    D[V[N]]
  ): Seq[Center] = Seq.empty

}