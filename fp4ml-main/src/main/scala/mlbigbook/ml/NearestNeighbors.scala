package mlbigbook.ml

import fif.Data
import mlbigbook.math.MathVectorOps

import scala.language.{ higherKinds, postfixOps, reflectiveCalls }
import scala.reflect.ClassTag

trait NearestNeighbors extends RankingModule {

  import Data.ops._

  override type Conf = Distance

  override def mkRanker[D[_]: Data](
    dist:  Distance,
    toVec: Vectorizer
  )(
    data: D[Item]
  ): Ranker = {
    val bothItemVec = data.map { item => (item, toVec.vectorize(item)) }
    limit => itemToRank => {
      val vecItemToRank = toVec.vectorize(itemToRank)
      bothItemVec
        .sortBy { case (item, vec) => dist(vec, vecItemToRank) }
        .take(limit)
        .map { case (item, _) => item }
        .toSeq
    }
  }

}

object NearestNeighbors {

  type Type[ItemToRank, Num, Vec[_]] = NearestNeighbors {
    type Item = ItemToRank
    type N = Num
    type V[_] = Vec[_]
  }

  def apply[ItemToRank, Num, Vec[_]](
    mathVecOps: MathVectorOps.Type[Num, Vec]
  )(
    implicit
    ctForI:  ClassTag[ItemToRank],
    ctForN:  ClassTag[Num],
    ctForVn: ClassTag[Vec[Num]]
  ): Type[ItemToRank, Num, Vec] =
    new NearestNeighbors {
      override type Item = ItemToRank
      override type N = Num
      override type V[_] = Vec[_]

      override lazy val vops = mathVecOps.asInstanceOf[MathVectorOps.Type[N, V]]

      override implicit lazy val ctI = ctForI
      override implicit lazy val ctN = ctForN
      override implicit lazy val ctVn = ctForVn.asInstanceOf[ClassTag[V[N]]]
    }

}