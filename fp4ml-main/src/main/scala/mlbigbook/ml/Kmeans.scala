package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ MathVectorOps, RandoMut }

import scala.annotation.tailrec
import scala.language.{ higherKinds, reflectiveCalls }
import scala.reflect.ClassTag

trait Kmeans extends ClusteringModule {

  /** Creates a pseudo-random number generator for the type N. */
  val mkRandomNumGen: () => RandoMut[N]

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

      println(
        s"""[center check: currIter=$currIter]
            |[ORIGINAL # ${currCenters.size}] ${currCenters.mkString("\t")}
            |[UPDATED  # ${updatedCenters.size}] ${updatedCenters.mkString("\t")}
         """.stripMargin
      )

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

  def apply[ItemToCluster, Num, Vec[_]](
    mathVectorOps: MathVectorOps.Type[Num, Vec],
    mkRando:       () => RandoMut[Num]
  )(
    implicit
    ctForI:  ClassTag[ItemToCluster],
    ctForN:  ClassTag[Num],
    ctForVn: ClassTag[Vec[Num]]
  ): Type[ItemToCluster, Num, Vec] = {

    //    val okVops: MathVectorOps.Type[Type[ItemToCluster, Num, Vec]#N, Type[ItemToCluster, Num, Vec]#V] =
    //      mathVectorOps
    //      mathVectorOps.asInstanceOf[MathVectorOps.Type[Type[ItemToCluster, Num, Vec]#N, Type[ItemToCluster, Num, Vec]#V]]

    //    val okCtVn: ClassTag[Type[ItemToCluster, Num, Vec]#V[Type[ItemToCluster, Num, Vec]#N]] =
    //      ctForVn
    //      ctForVn.asInstanceOf[ClassTag[Type[ItemToCluster, Num, Vec]#V[Type[ItemToCluster, Num, Vec]#N]]]

    new Kmeans {

      override type Item = ItemToCluster
      override type N = Num
      override type V[_] = Vec[_]

      override lazy val mkRandomNumGen = mkRando
      override lazy val vops = mathVectorOps.asInstanceOf[MathVectorOps.Type[N, V]]

      override implicit lazy val ctI = ctForI
      override implicit lazy val ctN = ctForN
      override implicit lazy val ctVn = ctForVn.asInstanceOf[ClassTag[V[N]]]
    }
  }

}
