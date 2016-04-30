package mlbigbook.data

import breeze.math.Semiring
import breeze.storage.Zero
import fif.Data
import mlbigbook.math.MathVectorOps

import scala.language.higherKinds

trait ClusteringModule {

  val vmod: VectorizerModule
  import vmod._
  import numVecMod._

  //  case class Center(id: String, mean: V[N])

  //  def mkVectorizer[D[_] : Data, T](data: D[T]): VectorizerModule.Type[Item, N, V]
  //
  //  def cluster[D[_] : Data, T](data: D[T]):

  //  type Item
  //
  //  type N
  //  implicit val num: Numeric[N]
  //  implicit val sr: Semiring[N]
  //  implicit val zero: Zero[N]
  //
  //  type V[_]
  //  implicit val vops: MathVectorOps[N, V]
  //
  //  type Vectorizer = Item => V[N]
  //  val vectorizer: Vectorizer
  //
  //  case class Center(id: String, mean: V[N])

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
  val toVec: Vectorizer
}

object VectorizerModule {
  //
  //  type Type[T, Number, Vec[_]] = VectorizerModule {
  //    type Item = T
  //
  //  }

}
