package mlbigbook.ml

import fif.Data
import mlbigbook.math.NumericX
import mlbigbook.ml.FeatureVectorSupport._

import scala.language.{ higherKinds, postfixOps }

trait Information {

  type Entropy
  type Label
  implicit def entropyIsNumeric: NumericX[Entropy]

  type EntropyPerFeature = Seq[Entropy]
  type SplitInfoPerFeature = Seq[Entropy]
  type GainRaitoPerFeature = Seq[Entropy]

  type FV = Seq[String]

  def gain[D[_]: Data](
    data: D[(FV, Label)]
  )(implicit fs: FeatureSpace): EntropyPerFeature

  def split[D[_]: Data](
    data: D[(FV, Label)]
  )(implicit fs: FeatureSpace): SplitInfoPerFeature

  final def gainRatio[D[_]: Data](
    data: D[(FV, Label)]
  )(implicit fs: FeatureSpace): GainRaitoPerFeature = {

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