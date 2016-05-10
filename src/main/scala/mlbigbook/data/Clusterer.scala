package mlbigbook.data

import breeze.math.Semiring
import breeze.storage.Zero
import fif.Data
import mlbigbook.data.DistanceTM.Type
import mlbigbook.math.MathVectorOps

import scala.annotation.tailrec
import scala.language.{ reflectiveCalls, higherKinds }
import scala.reflect.ClassTag
import scala.util.Random

abstract class RandoMut[N: Numeric] {
  def next(): N
}

trait Clusterer extends ActionModule {

  type Item
  type N
  type V[_]
  val v: VectorizerTM.Type[Item, N, V]
  val d: DistanceTM.Type[N, V]

  protected[Clusterer] implicit val _0: Numeric[N] = v.vops.n
  protected[Clusterer] implicit val _1: Semiring[N] = v.vops.s
  protected[Clusterer] implicit val _2: Zero[N] = v.vops.z

  case class Center(id: String, mean: V[N])

  final def cluster[D[_]: Data](
    conf:  ClusteringConf,
    dist:  d.Distance,
    mkVec: MkVectorizer.Type[Item, N, V]
  )(data: D[Item]): Seq[Center] =
    cluster(conf, dist, mkVec(data))(data)

  def cluster[D[_]: Data](
    conf:  ClusteringConf,
    dist:  d.Distance,
    toVec: v.Vectorizer
  )(data: D[Item]): Seq[Center]

}

trait Kmeans extends Clusterer {

  implicit def ct: ClassTag[N]

  val mkRandomNumGen: () => RandoMut[N]

  def initialize(
    nClusters:   Int,
    nDimensions: Int
  ): Seq[Center] = {
    val r = mkRandomNumGen()
    (0 until nClusters)
      .map { id =>
        Center(
          id = id.toString,
          mean = v.vops.map(v.vops.ones(nDimensions)) {
            one => v.vops.n.times(one, r.next())
          }
        )
      }
      .toSeq
  }

  override def cluster[D[_]: Data](
    conf:  ClusteringConf,
    dist:  d.Distance,
    toVec: v.Vectorizer
  )(data: D[v.Item]): Seq[Center] =
    cluster_h(
      conf,
      dist,
      toVec,
      0,
      data,
      initialize(conf.nClusters, toVec.dimensionality)
    )

  @tailrec
  @inline
  private[this] def cluster_h[D[_]: Data](
    conf:        ClusteringConf,
    dist:        d.Distance,
    toVec:       v.Vectorizer,
    currIter:    Int,
    data:        D[v.Item],
    currCenters: Seq[Center]
  ): Seq[Center] =

    if (currIter >= conf.maxIterations)
      currCenters

    else {
      val updatedCenters = updateCenters(dist, toVec, currCenters)

      val sumSquaredChangeInMeansBetweenIters =
        currCenters.zip(updatedCenters)
          .foldLeft(0.0) {
            case (accum, (existing, updated)) =>
              val d = math.abs(
                v.vops.n.toDouble(
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

  protected def updateCenters[D[_]: Data](
    dist:    d.Distance,
    toVec:   v.Vectorizer,
    centers: Seq[Center]
  ): Seq[Center]

  // CRITICAL to have these *trivial* overrides...
  override type N = Clusterer#N
  override type Item = Clusterer#Item
  override type V[_] = Clusterer#V[_]
}

abstract class X extends Kmeans {

  val x = _0

}

case class ClusteringConf(
  nClusters:     Int,
  tolerance:     Double,
  maxIterations: Int
)

trait MkVectorizer extends ActionModule {

  type T
  type N
  type V[_]
  implicit val v: VectorizerTM.Type[T, N, V]

  def apply[D[_]: Data](data: D[T]): v.Vectorizer
}

object MkVectorizer {
  type Type[Item, Num, Vec[_]] = MkVectorizer {
    type T = Item
    type N = Num
    type V[_] = Vec[_]
  }

}

trait DistanceTM extends TypeModule {

  type N
  type V[_]
  implicit val vops: MathVectorOps[N, V]

  type Distance = (V[N], V[N]) => N
}

object DistanceTM {
  type Type[Num, Vec[_]] = DistanceTM {
    type N = Num
    type V[_] = Vec[_]
  }
}

trait ActionModule

trait TypeModule

trait VectorizerTM extends TypeModule {

  type Item
  type N
  type V[_]
  implicit val vops: MathVectorOps[N, V]

  type Vectorizer = {
    val apply: Item => V[N]
    val dimensionality: Int
  }
}

object VectorizerTM {
  type Type[T, Num, Vec[_]] = VectorizerTM {
    type Item = T
    type N = Num
    type V[_] = Vec[_]
  }

}
