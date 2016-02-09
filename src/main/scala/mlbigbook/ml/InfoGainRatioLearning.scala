package mlbigbook.ml

import fif.Data
import mlbigbook.ml.FeatureVectorSupport.FeatureSpace

import scala.language.{ higherKinds, postfixOps }

object InfoGainRatioLearning extends FeatureImportance {

  override def apply[D[_]: Data](
    data: D[(Seq[String], Boolean)]
  )(implicit fs: FeatureSpace) =
    fs.features.zip(InformationBinaryLabel.gainRatio(data))
}