package mlbigbook.newthings

import breeze.math.Semiring
import breeze.storage.Zero
import fif.Data
import mlbigbook.math.MathVectorOps

import scala.language.higherKinds

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

}