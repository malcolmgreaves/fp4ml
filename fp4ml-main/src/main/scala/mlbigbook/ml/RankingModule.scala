package mlbigbook.ml

import fif.Data

import scala.language.{higherKinds, postfixOps, reflectiveCalls}

trait RankingModule extends ItemNumVecModule {

  type Vectorizer = {
    val vectorize: Item => V[N]
    val nDimensions: Int
  }

  type Distance = (V[N], V[N]) => N

  type Ranker = Int => Item => Seq[Item]

  type Conf

  final def mkRanker[D[_]: Data](
      c: Conf,
      mkVectorizer: D[Item] => Vectorizer
  )(data: D[Item]): Ranker =
    mkRanker(c, mkVectorizer(data))(data)

  def mkRanker[D[_]: Data](
      c: Conf,
      toVec: Vectorizer
  )(
      data: D[Item]
  ): Ranker

}
