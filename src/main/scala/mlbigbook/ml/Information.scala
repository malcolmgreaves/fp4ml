package mlbigbook.ml

import breeze.linalg.{ DenseVector, Vector }
import fif.Data
import mlbigbook.math.{ NumericX, NumericConversion, OnlineMeanVariance, VectorOpsT }
import mlbigbook.ml.FeatureVectorSupport._

import scala.language.{ higherKinds, postfixOps }

trait FeatVecOps[F] {

  def contains(featureName: String)(fvec: F)(implicit fs: FeatureSpace): Boolean

}

trait Information {

  type Entropy
  type Label
  implicit def entropyIsNumeric: NumericX[Entropy]

  type EntropyPerFeature = Seq[Entropy]
  type SplitInfoPerFeature = Seq[Entropy]
  type GainRaitoPerFeature = Seq[Entropy]

  type FV = Seq[String]

  implicit def fvOps: FeatVecOps[FV]

  def gain[D[_]: Data](
    data: D[(FV, Label)]
  )(
    implicit
    fs: FeatureSpace
  ): EntropyPerFeature

  def split[D[_]: Data](
    data: D[(FV, Label)]
  )(
    implicit
    fs: FeatureSpace
  ): SplitInfoPerFeature

  def gainRatio[D[_]: Data](
    data: D[(FV, Label)]
  )(
    implicit
    fs: FeatureSpace
  ): GainRaitoPerFeature = {

    val infoGain = gain(data)
    val splitInfo = split(data)

    infoGain.zip(splitInfo)
      .map {
        case (ig, si) => entropyIsNumeric.div(ig, si)
      }
  }

}

object Information {

  import Data.ops._

  @inline def log2(value: Double): Double =
    math.log(value) / logBase10of2

  private[this] val logBase10of2 = math.log(2.0)

  def partition[D[_]: Data, FV, Label](
    data:          D[(FV, Label)],
    viewValueOfFv: FV => String,
    value:         String
  ): D[(FV, Label)] =
    data.filter {
      case (fv, _) => viewValueOfFv(fv) == value
    }

}

object InformationBinaryLabel extends Information {

  import Data.ops._

  override type Entropy = Double
  override type Label = Boolean

  override implicit val entropyIsNumeric =
    NumericX.Implicits.DoubleX

  override def gain[D[_]: Data](
    data: D[(FV, Label)]
  )(
    implicit
    fs: FeatureSpace
  ) = {

    val totalSize = data.size.toDouble
    val totalEntropy = entropyOf(data)

    fs.features
      .map { featureName =>
        val featureIndex = fs.feat2index(featureName)
        val distinctValues = fs.categorical2values(featureName)

        val entropyOfFeatureByDistinct =
          distinctValues
            .map { distinct =>

              val subset =
                Information.partition(
                  data,
                  (fv: Seq[String]) => fv(featureIndex),
                  distinct
                )

              val proportionOfDistinct = subset.size.toDouble / totalSize

              proportionOfDistinct * entropyOf(subset)
            }
            .sum

        val informationGain = totalEntropy - entropyOfFeatureByDistinct

        (featureIndex, informationGain)
      }
      .sortBy { case (featureIndex, _) => featureIndex }
      .map { case (_, informationGain) => informationGain }
  }

  def entropyOf[D[_]: Data](data: D[(Seq[String], Label)]): Entropy = {

    val (nNeg, nPos) =
      data.aggregate((0l, 0l))(
        {
          case ((nNegAgg, nPosAgg), (_, label)) =>
            if (label)
              (nNegAgg, nPosAgg + 1l)
            else
              (nNegAgg + 1l, nPosAgg)
        },
        {
          case ((nNeg0, nPos0), (nNeg1, nPos1)) =>
            (nNeg0 + nNeg1, nPos0 + nPos1)
        }
      )

    val pNeg = nNeg.toDouble / nPos
    val pPos = 1.0 - pNeg

    -(pNeg * Information.log2(pNeg) + pPos * Information.log2(pPos))
  }

  override def split[D[_]: Data](
    data: D[(FV, Label)]
  )(
    implicit
    fs: FeatureSpace
  ) = {

    //    data
    //      .aggregate(0)(
    //        { ??? },
    //        { ??? }
    //      )
    //
    //    // for each distinct feature value V:
    //    //   a = calculate # of times in dataset that an example had V occur
    //    //   b = calculate total # of examples
    //    //   ++ (a/b) * log_2(a/b)
    //
    //    fs.features
    //      .map { featureName =>
    //
    //      }

    ???
  }

  override implicit def fvOps: FeatVecOps[InformationBinaryLabel.FV] =
    ???
}