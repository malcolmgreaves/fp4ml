//package mlbigbook.data
//
//import breeze.math.Semiring
//import breeze.storage.Zero
//import fif.Data
//import mlbigbook.data.DistanceTM.Type
//import mlbigbook.math.MathVectorOps
//
//import scala.language.higherKinds
//import scala.reflect.ClassTag
//import scala.util.Random
//
//abstract class RandoMut[N: Numeric] {
//  def next(): N
//}
//
//trait Kmeans extends Clusterer {
//
//  implicit def ct: ClassTag[N]
//
//  def foo(): V[N] = {
//    v.vops.map[N](v.vops.ones(10))(identity[N])(
//      ct,
//      v.vops.n,
//      v.vops.z
//    )
//  }
//
//  def initialize(
//    nClusters:   Int,
//    nDimensions: Int
//  )(implicit r: RandoMut[N]): Seq[Center] =
//    (0 until nClusters)
//      .map { id =>
//        implicit val _0: Numeric[N] = v.vops.n
//        implicit val _1: Semiring[N] = v.vops.s
//        implicit val _2: Zero[N] = v.vops.z
//
//
//
//
////        val v1: V[N] = v.vops.ones(nDimensions)
////        val v1: V[N] = null.asInstanceOf[V[N]]
////        val v2: V[N] = v.vops.map[N](
////          v1
//////          null.asInstanceOf[V[N]]
//////          v1.asInstanceOf[V[Kmeans.this.N]]
////        )(_ => r.next())
//        Center(
//          id = id.toString,
//          mean = foo()
//        )
//      }
//      .toSeq
//
//  override def cluster[D[_]: Data](
//    conf:  ClusteringConf,
//    dist:  d.Distance,
//    toVec: v.Vectorizer
//  )(data: D[v.Item]): Seq[Center] = ???
//
//}
//
//case class ClusteringConf(
//  nClusters:     Int,
//  tolerance:     Double,
//  maxIterations: Int
//)
//
//trait MkVectorizer extends ActionModule {
//
//  type T
//  type N
//  type V[_]
//  implicit val v: VectorizerTM.Type[T, N, V]
//
//  def apply[D[_]: Data](data: D[T]): v.Vectorizer
//}
//
//object MkVectorizer {
//  type Type[Item, Num, Vec[_]] = MkVectorizer {
//    type T = Item
//    type N = Num
//    type V[_] = Vec[_]
//  }
//
//}
//
//trait Clusterer extends ActionModule {
//
//  type Item
//  type N
//  type V[_]
//  val v: VectorizerTM.Type[Item, N, V]
//  val d: DistanceTM.Type[N, V]
//
//
//  case class Center(id: String, mean: V[N])
//
//  final def cluster[D[_]: Data](
//    conf:  ClusteringConf,
//    dist:  d.Distance,
//    mkVec: MkVectorizer.Type[Item, N, V]
//  )(data: D[Item]): Seq[Center] =
//    cluster(conf, dist, mkVec(data))(data)
//
//  def cluster[D[_]: Data](
//    conf:  ClusteringConf,
//    dist:  d.Distance,
//    toVec: v.Vectorizer
//  )(data: D[Item]): Seq[Center]
//
//}
//
//trait DistanceTM extends TypeModule {
//
//  type N
//  type V[_]
//  implicit val vops: MathVectorOps[N, V]
//
//  type Distance = (V[N], V[N]) => N
//}
//
//object DistanceTM {
//  type Type[Num, Vec[_]] = DistanceTM {
//    type N = Num
//    type V[_] = Vec[_]
//  }
//}
//
//trait ActionModule
//
//trait TypeModule
//
//trait VectorizerTM extends TypeModule {
//
//  type Item
//  type N
//  type V[_]
//  implicit val vops: MathVectorOps[N, V]
//
//  type Vectorizer = Item => V[N]
//}
//
//object VectorizerTM {
//  type Type[T, Num, Vec[_]] = VectorizerTM {
//    type Item = T
//    type N = Num
//    type V[_] = Vec[_]
//  }
//
//}
