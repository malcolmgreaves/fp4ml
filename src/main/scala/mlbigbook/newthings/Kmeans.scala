package mlbigbook.newthings

import fif.Data
import mlbigbook.math.MathVectorOps

import scala.annotation.tailrec
import scala.language.{ higherKinds, reflectiveCalls }
import scala.reflect.ClassTag

trait Kmeans extends Clustering {

  /** Creates a pseudo-random number generator for the type N. */
  val mkRandomNumGen: () => RandoMut[N]

  // ClassTag evidence necessary for MathVectorOps, Data abstractions
  protected implicit val ctN: ClassTag[N]

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
  ): Seq[Center] =
    data.zip(assign(centers, dist)(data))
      .groupBy { case (_, assignment) => assignment }
      .map {
        case (label, bothDataAndLabel) =>

          val summed =
            bothDataAndLabel
              .foldLeft(vops.zeros(toVec.nDimensions)) {
                case (summing, (vector, _)) =>
                  vops.addV(summing, vector)
              }

          val newMean =
            vops.divS(
              summed,
              implicitly[Numeric[N]].fromInt(bothDataAndLabel.size)
            )

          Center(
            id = label,
            mean = newMean
          )
      }
      .toSeq

}

object Kmeans {

  type Type[ItemToCluster, Num, Vec[_]] = Kmeans {
    type Item = ItemToCluster
    type N = Num
    type V[_] = Vec[_]
  }

  def apply[ItemToCluster, Num: Numeric: ClassTag, Vec[_]](
    mathVectorOps: MathVectorOps[Num, Vec],
    mkRando:       () => RandoMut[Num]
  )(
    implicit
    ctForVn: ClassTag[Vec[Num]]
  ): Type[ItemToCluster, Num, Vec] = {
    val ctForN = implicitly[ClassTag[Num]]
    new Kmeans {

      override type N = Num
      override type V[_] = Vec[_]
      override type Item = ItemToCluster

      override lazy val mkRandomNumGen = mkRando
      override lazy val vops: MathVectorOps[N, V] =
        mathVectorOps
//        mathVectorOps.asInstanceOf[MathVectorOps[N,V]]

      override protected implicit lazy val ctN = ctForN
      override protected implicit lazy val ctVn: ClassTag[V[N]] =
        ctForVn
//        ctForVn.asInstanceOf[ClassTag[V[N]]]
    }
  }

}