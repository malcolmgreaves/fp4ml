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

trait MkVectorizerModule {

  val vmod: VectorizerTypeModule
  import vmod._

  def apply[D[_]: Data](data: D[Item]): Vectorizer
}

trait ClusteringModule {

  val vmod: VectorizerTypeModule
  import vmod._
  import numVecMod._

  val dmod: DistanceTypeModule
  import dmod._

  case class Center(id: String, mean: V[N])

  def cluster[D[_]: Data](
    conf:  ClusteringConf,
    dist:  Distance,
    toVec: Vectorizer
  )(data: D[Item]): Seq[Center]

}

trait DistanceTypeModule {

  val numVecMod: NumericalVectorTypeModule
  import numVecMod._

  type Distance = (V[N], V[N]) => N
}

trait NumericalVectorTypeModule {

  type N
  implicit val num: Numeric[N]
  implicit val sr: Semiring[N]
  implicit val zero: Zero[N]

  type V[_]
  implicit val vops: MathVectorOps[N, V]

}

object NumericalVectorTypeModule {

  type Type[Num, Vec[_]] = NumericalVectorTypeModule {
    type N = Num
    type V[_] = Vec[_]
  }
}

trait VectorizerTypeModule {

  type Item

  val numVecMod: NumericalVectorTypeModule
  import numVecMod._

  type Vectorizer = Item => V[N]
}

object VectorizerTypeModule {

  type Type[T, Num, Vec[_]] = VectorizerTypeModule {
    type Item = T
    val numVecMod: NumericalVectorTypeModule.Type[Num, Vec]
  }

}
