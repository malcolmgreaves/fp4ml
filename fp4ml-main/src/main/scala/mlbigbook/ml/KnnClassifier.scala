package mlbigbook.ml

import fif.Data
import mlbigbook.math.{MathVectorOps, Val, Argmax}

import scala.language.{higherKinds, postfixOps, reflectiveCalls}
import scala.reflect.ClassTag

trait KnnClassifier extends ClassificationModule {

  final type NeighborhoodSize = Int

  type Distance = (V[N], V[N]) => N
  override final type Conf = (NeighborhoodSize, Distance)

  implicit val labelHash: Hashable[Label]

  private[this] lazy val nnRankMod =
    NearestNeighbors[(Item, Label), N, V](vops)

  def train[D[_]: Data](
      c: (NeighborhoodSize, Distance),
      toVec: Vectorizer
  )(data: D[(Item, Label)]): Classifier = {

    val (nSize, dist) = c

    val nnRanker = nnRankMod.mkRanker[D](
      dist.asInstanceOf[nnRankMod.Distance],
      new {
        val vectorize: ((Item, Label)) => V[N] = {
          case (item, _) => toVec.vectorize(item)
        }
        val nDimensions = toVec.nDimensions
      }
    )(data)

    itemToClassify =>
      {

        val neighborhood = nnRanker(nSize)((itemToClassify, emptyLabel))

        val votesForNeighborhood: Map[Label, Int] =
          neighborhood.foldLeft(CustomHashMap.empty[Label, Int]) {
            case (label2count, (item, label)) =>
              if (label2count contains label)
                (label2count - label) + (label -> (label2count(label) + 1))
              else
                label2count + (label -> 1)
          }

        import fif.ImplicitCollectionsData.seqIsData
        import Val.Implicits.tupleValIn2nd

        Argmax(votesForNeighborhood.toSeq).fold { emptyLabel } {
          case (majorityLabel, _) => majorityLabel
        }
      }
  }

}

object KnnClassifier {

  type Type[Input, L, Num, Vec[_]] = KnnClassifier {
    type Item = Input
    type Label = L
    type N = Num
    type V[_] = Vec[_]
  }

  def apply[Input, L, Num, Vec[_]](
      mathVecOps: MathVectorOps.Type[Num, Vec],
      representsNoLabel: L
  )(
      implicit ctForI: ClassTag[Input],
      ctForN: ClassTag[Num],
      ctForVn: ClassTag[Vec[Num]],
      lh: Hashable[L]
  ): Type[Input, L, Num, Vec] =
    new KnnClassifier {
      override type Item = Input
      override type N = Num
      override type V[_] = Vec[_]
      override type Label = L

      override lazy val emptyLabel = representsNoLabel
      override lazy val vops =
        mathVecOps.asInstanceOf[MathVectorOps.Type[N, V]]

      override implicit lazy val labelHash = lh

      override implicit lazy val ctI = ctForI
      override implicit lazy val ctN = ctForN
      override implicit lazy val ctVn = ctForVn.asInstanceOf[ClassTag[V[N]]]
    }

}
