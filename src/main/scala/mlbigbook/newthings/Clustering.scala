package mlbigbook.newthings

import breeze.math.Semiring
import breeze.storage.Zero
import fif.Data
import mlbigbook.math.MathVectorOps

import scala.language.{ postfixOps, higherKinds, reflectiveCalls }
import scala.reflect.ClassTag

trait Clustering {

  type Item
  type N
  type V[_]

  // vops serves as a type class for numeric vector operations
  // having an instance of type MathVectorOps[N,V] implies constraints on N and V
  val vops: MathVectorOps[N, V]

  // we can get these type classes for N
  protected[Clustering] implicit lazy val _0: Numeric[N] = vops.n
  protected[Clustering] implicit lazy val _1: Semiring[N] = vops.s
  protected[Clustering] implicit lazy val _2: Zero[N] = vops.z

  type Vectorizer = {
    val vectorize: Item => V[N]
    val nDimensions: Int
  }

  type Distance = (V[N], V[N]) => N

  case class Center(id: String, mean: V[N])

  final def cluster[D[_]: Data](
    conf:         ClusteringConf,
    dist:         Distance,
    mkVectorizer: D[Item] => Vectorizer
  )(data: D[Item]): Seq[Center] =
    cluster(conf, dist, mkVectorizer(data))(data)

  def cluster[D[_]: Data](
    conf:  ClusteringConf,
    dist:  Distance,
    toVec: Vectorizer
  )(data: D[Item]): Seq[Center]

  // required for assign definition
  protected[Clustering] implicit val ctVn: ClassTag[V[N]]

  import Data.ops._

  final def assign[D[_]: Data](
    centers:    Seq[Center],
    distance:   Distance,
    vectorizer: Vectorizer
  )(
    data: D[Item]
  ): D[String] =
    assign(centers, distance)(
      data map { vectorizer.vectorize }
    )

  final def assign[D[_]: Data](
    centers:  Seq[Center],
    distance: Distance
  )(
    data: D[V[N]]
  ): D[String] =

    if (centers isEmpty)
      data map { _ => "" }

    else if (centers.size == 1) {
      val label = centers.head.id
      data map { _ => label }

    } else {

      val lessThan = implicitly[Numeric[N]].lt _

      val initialLabel = centers.head.id
      val restCenters = centers.slice(1, centers.size)

      data map { v =>

        val (nearestLabel, _) =
          restCenters.foldLeft(initialLabel, distance(centers.head.mean, v)) {

            case (currChampion @ (minLabel, minDistance), center) =>

              val distToCenter = distance(center.mean, v)
              if (lessThan(distToCenter, minDistance))
                (center.id, distToCenter)
              else
                currChampion
          }

        nearestLabel
      }
    }

}