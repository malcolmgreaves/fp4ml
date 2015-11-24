package mlbigbook.ml

import breeze.linalg.{ DenseVector, Vector }
import fif.Data
import mlbigbook.math.{ NumericConversion, OnlineMeanVariance, VectorOpsT }

import scala.language.{ higherKinds, postfixOps }

object InformationSimpleFv {

  import FeatureVectorSupport._
  import fif.Data.ops._

  /*
    Strategy for continuous variables:

      (1) Calculate mean & variance for each continous variable.
      (2) Construct a gaussian with ^^.
      (3) Calculate entropy of each estimated gaussian.
   */
  def entropyContinous[D[_]: Data, N: NumericConversion, V[_] <: Vector[_]](
    realOnly: D[V[N]]
  )(
    implicit
    ops: VectorOpsT[N, V],
    fs:  FeatureSpace
  ): Map[String, Double] = {

    val statsForAllRealFeatures =
      OnlineMeanVariance.batch[D, N, V](realOnly)

    val gf = {
      import MathOps.Implicits._
      GaussianFactory[Double]
    }

    val realFeat2gaussian: Map[String, GaussianFactory[Double]#Gaussian] = {
      val toDouble = NumericConversion[N].numeric.toDouble _
      (0 until statsForAllRealFeatures.mean.size)
        .zip(fs.realFeatNames)
        .map {
          case (index, realFeatName) =>
            val g = new gf.Gaussian(
              mean = toDouble(ops.valueAt(statsForAllRealFeatures.mean)(index)),
              variance = toDouble(ops.valueAt(statsForAllRealFeatures.variance)(index)),
              stddev = toDouble(ops.valueAt(statsForAllRealFeatures.variance)(index))
            )
            (realFeatName, g)
        }
        .toMap
    }

    realFeat2gaussian
      .map {
        case (realFeatName, gaussian) =>
          (realFeatName, entropyOf(gaussian))
      }
  }

  /*
    Strategy for discrete variables:

    for each feature
      - count events

    for each feature
      for each event in feature:
        - calculate P(event) ==> # events / total
        - entropy(feature)   ==> - sum( p(event) * log_2( p(event) ) )
   */
  def entropyCategorical[D[_]: Data](
    categoricalOnly: D[Seq[String]]
  )(implicit fs: FeatureSpace): Map[String, Double] = {

    val catFeat2event2count =
      categoricalOnly
        .aggregate(Map.empty[String, Map[String, Long]])(
          {
            case (feat2event2count, featureValues) =>
              featureValues.zip(fs.catFeatNames)
                .foldLeft(feat2event2count) {
                  case (m, (value, name)) =>
                    Counting.incrementNested(
                      m,
                      name,
                      value
                    )
                }
          },
          Counting.combineNested[String, String, Long]
        )

    catFeat2event2count
      .map {
        case (feature, event2count) =>

          val entropyOfFeature = {
            val totalEventCount = event2count.values.sum.toDouble
            -event2count.foldLeft(0.0) {
              case (sum, (_, count)) =>
                val probabilityEvent = count / totalEventCount
                probabilityEvent * logBase2(probabilityEvent)
            }
          }

          (feature, entropyOfFeature)
      }
  }

  def entropy[D[_]: Data, FV](
    data: D[FV]
  )(
    implicit
    fs:   FeatureSpace,
    isFv: FV => FeatVec
  ): Seq[Double] = {

    val realFeat2entropy = {

      val realOnly: D[DenseVector[Double]] =
        data.map { fv =>
          val realValues =
            fs.realIndices
              .map { index =>
                fv(index) match {
                  case Real(v) => v
                  case Categorical(_) =>
                    throw new IllegalStateException(
                      s"Violation of FeatureSpace contract: feature at index $index is categorical, expecting real"
                    )
                }
              }
              .toArray
          DenseVector(realValues)
        }

      import NumericConversion.Implicits._
      import VectorOpsT.Implicits._
      entropyContinous(realOnly)
    }

    val catFeat2Entropy = {

      val categoricalOnly: D[Seq[String]] =
        data.map { fv =>
          fs.catIndices.map { index =>
            fv(index) match {
              case Categorical(v) => v
              case Real(_) =>
                throw new IllegalStateException(
                  s"Violation of FeatureSpace contract: feature at index $index is real, expecting categorical"
                )
            }
          }
        }

      entropyCategorical(categoricalOnly)
    }

    // put calculated entropy for both continuous and categorical features into
    // the same (feature name --> entropy) mapping
    fs.feat2index
      .map {
        case (featureName, index) =>
          if (catFeat2Entropy contains featureName)
            (catFeat2Entropy(featureName), index)
          else
            (realFeat2entropy(featureName), index)
      }
      .toSeq
      .sortBy { case (_, index) => index }
      .map { case (entropy, _) => entropy }
  }

  val logBase2 = logBaseX(2.0) _

  val logBaseE = logBaseX(math.E) _

  def logBaseX(base: Double)(value: Double): Double =
    math.log(value) / math.log(base)

  private[this] val gaussianEntropyConst = math.sqrt(2.0 * math.Pi * math.E)

  def entropyOf(g: GaussianFactory[Double]#Gaussian): Double =
    logBaseE(gaussianEntropyConst * g.stddev)

}