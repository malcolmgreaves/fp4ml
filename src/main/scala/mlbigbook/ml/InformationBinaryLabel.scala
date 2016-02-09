package mlbigbook.ml

import fif.Data
import mlbigbook.math.NumericX
import mlbigbook.ml.FeatureVectorSupport._

import scala.language.{ higherKinds, postfixOps }

object InformationBinaryLabel extends Information {

  import Data.ops._

  override type Entropy = Double
  override type Label = Boolean

  override implicit val entropyIsNumeric =
    NumericX.Implicits.DoubleX

  override def gain[D[_]: Data](
    data: D[(FV, Label)]
  )(implicit fs: FeatureSpace) = {

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

  private[this] def mkFeatDistCounts(fs: FeatureSpace): Seq[Array[Long]] =
    fs.features
      .map { featureName =>
        Array.fill[Long](fs.categorical2values(featureName).size)(0l)
      }

  override def split[D[_]: Data](
    data: D[(FV, Label)]
  )(implicit fs: FeatureSpace) = {

    val distinctPerFeatIndex: Seq[Map[String, Int]] =
      fs.features
        .map { featureName =>
          fs.categorical2values(featureName)
            .zipWithIndex
            .toMap
        }

    val nExamples = data.size.toDouble

    data
      .aggregate(mkFeatDistCounts(fs))(
        {
          case (fdlc, (fv, label)) =>
            // MUTATION WARNING
            // We are mutating the inner array for each feature as we are doing
            // a sequence of updates.
            // It is more efficient to use mutable state and control its outside
            // scope.
            fdlc.zipWithIndex
              .foreach {
                case (featCountsArray, fIndex) =>
                  val featureValue = fv(fIndex)
                  val distinctIndex = distinctPerFeatIndex(fIndex)(featureValue)
                  featCountsArray(distinctIndex) += 1l
              }
            fdlc
        },
        {
          case (fdlc1, fdlc2) =>
            fdlc1.zip(fdlc2)
              .map {
                case (featCountsArray1, featCountsArray2) =>
                  // WLOG we're going to mutate the first one and evaluate to it
                  featCountsArray1.indices
                    .foreach { fIndex =>
                      featCountsArray1(fIndex) += featCountsArray2(fIndex)
                    }
                  featCountsArray1
              }
        }
      )
      .map { featCountsArray =>
        // the negative sign here is important !
        -featCountsArray.indices
          .foldLeft(0.0) {
            case (sum, countForDistinct) =>
              val frac = countForDistinct.toDouble / nExamples
              frac * Information.log2(frac)
          }
      }
  }

}