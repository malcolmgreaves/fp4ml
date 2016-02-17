package mlbigbook.ml

import fif.Data
import Data.ops._
import mlbigbook.math.VectorOpsT

import scala.language.higherKinds
import scala.reflect.ClassTag

object CutPoint {

  /**
   * Finds the value that, when used to partition the input data, maximizes
   * the information gain of the resulting partitions.
   */
  def apply[D[_]: Data, V[_], N: Numeric: ClassTag](data: D[(V[N], Boolean)])(
    implicit
    fs:   FeatureSpace,
    vops: VectorOpsT[N, V]
  ): Seq[N] =
    if (data isEmpty)
      Seq.empty[N]

    else
      (0 until fs.size).map { fIndex =>
        apply(
          data.map {
            case (vec, label) => (vops.valueAt(vec)(fIndex), label)
          }
        )
      }

  def apply[D[_]: Data, N: Numeric: ClassTag](data: D[(N, Boolean)]): N =
    if (data isEmpty)
      implicitly[Numeric[N]].zero
    else
      rawCutPointAlgo(data)
        .map { cpInfo => cpInfo.cutPoint }
        .getOrElse(implicitly[Numeric[N]].zero)

  def rawCutPointAlgo[D[_]: Data, N: Numeric: ClassTag](
    data: D[(N, Boolean)]
  ): Option[CutPointInfo[D, N]] = {

    // obtain all cut points:
    // each time the label sequence (ordered on increasing feature value)
    // changes is a cut point candidate

    val allCutPoints: Seq[N] =
      data
        .sortBy { case (value, _) => value }
        .aggregate((Seq.empty[N], Option.empty[Boolean]))(
          {
            case (same @ (cps, maybeLastLabel), (value, label)) =>
              maybeLastLabel match {
                case Some(lastLabel) =>
                  if (lastLabel != label)
                    (cps :+ value, Some(label))
                  else
                    (cps, maybeLastLabel)

                case None =>
                  (cps :+ value, Some(label))
              }
          },
          {
            case ((cps1, _), (cps2, _)) =>
              (cps1 ++ cps2, None)
          }

        )._1 // we only care about the Seq[N] at the end of the day

    // partition
    // calculate information gain increase from each cut point
    // select the one that achieves the maximium increase

    import EqualityT.Implicits._

    val totalEntropy = Entropy(data)

    val totalSize = data.size.toDouble

    val cpWithInfoIncrease =
      allCutPoints
        .map { cp =>

          val (s1, s2) = partition(data, cp)

          val entS1 = Entropy(s1)
          val scaledS1Entropy = (s1.size / totalSize) * entS1

          val entS2 = Entropy(s2)
          val scaledS2Entropy = (s2.size / totalSize) * entS2

          val informationGainFromSplit = totalEntropy - scaledS1Entropy - scaledS2Entropy

          (
            CutPointInfo(
              totalEntropy = totalEntropy,
              cutPoint = cp,
              infoGainOfCp = informationGainFromSplit,
              s1 = s1,
              s1Entropy = entS1,
              s2 = s2,
              s2Entropy = entS2
            ),
            informationGainFromSplit
          )
        }

    import fif.ImplicitCollectionsData._
    implicit val _ = TupleVal2[CutPointInfo[D, N]]
    Argmax(cpWithInfoIncrease)
      .map { case (cpInfo, _) => cpInfo }
  }

  def partition[D[_]: Data, N: Numeric](
    data: D[(N, Boolean)],
    cp:   N
  ): (D[(N, Boolean)], D[(N, Boolean)]) = {

    val lessThan = implicitly[Numeric[N]].lt _

    val lessThanCp =
      data.filter { case (value, _) => lessThan(value, cp) }

    val greaterThanOrEqualToCp =
      data.filter { case (value, _) => !lessThan(value, cp) }

    (lessThanCp, greaterThanOrEqualToCp)
  }

}