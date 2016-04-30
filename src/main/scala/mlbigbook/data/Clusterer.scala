package mlbigbook.data

import breeze.math.Semiring
import breeze.storage.Zero
import fif.Data
import mlbigbook.math.MathVectorOps

import scala.language.higherKinds

case class ClusteringConf(
  nClusters:     Int,
  tolerance:     Double,
  maxIterations: Int
)

trait ClusteringModule {

  val vmod: VectorizerModule
  import vmod._
  import numVecMod._

  def mkVectorizer[D[_]: Data, T](data: D[T]): Vectorizer

  val dmod: DistanceModule
  import dmod._

  case class Center(id: String, mean: V[N])

  final def cluster[D[_]: Data, T](
    conf: ClusteringConf,
    dist: Distance
  )(data: D[T]): Seq[Center] =
    cluster(conf, dist, mkVectorizer(data))(data)

  def cluster[D[_]: Data, T](
    conf:  ClusteringConf,
    dist:  Distance,
    toVec: Vectorizer
  )(data: D[T]): Seq[Center]

}

trait DistanceModule {

  val numVecMod: NumericalVectorModule
  import numVecMod._

  type Distance = (V[N], V[N]) => N
}

trait NumericalVectorModule {

  type N
  implicit val num: Numeric[N]
  implicit val sr: Semiring[N]
  implicit val zero: Zero[N]

  type V[_]
  implicit val vops: MathVectorOps[N, V]

}

object NumericalVectorModule {

  type Type[Num, Vec[_]] = NumericalVectorModule {
    type N = Num
    type V[_] = Vec[_]
  }
}

trait VectorizerModule {

  type Item

  val numVecMod: NumericalVectorModule
  import numVecMod._

  type Vectorizer = Item => V[N]
}

object VectorizerModule {

  type Type[T, Num, Vec[_]] = VectorizerModule {
    type Item = T
    val numVecMod: NumericalVectorModule.Type[Num, Vec]
  }

}
