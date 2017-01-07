package mlbigbook.ml

import fif.Data

import scala.annotation.tailrec
import scala.language.{higherKinds, postfixOps, reflectiveCalls}
import scala.reflect.ClassTag

trait ClassificationModule extends ItemNumVecModule {

  type Label
  val emptyLabel: Label

  type Classifier = Item => Label

  type Vectorizer = {
    val vectorize: Item => V[N]
    val nDimensions: Int
  }

  type Conf

  import Data.ops._

  final def train[D[_]: Data](
      c: Conf,
      mkVectorizer: D[(Item, Label)] => Vectorizer
  )(data: D[(Item, Label)]): Classifier =
    train(
      c,
      mkVectorizer { data }
    )(data)

  def train[D[_]: Data](
      c: Conf,
      toVec: Vectorizer
  )(data: D[(Item, Label)]): Classifier

}
