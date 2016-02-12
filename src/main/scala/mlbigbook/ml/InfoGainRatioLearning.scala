package mlbigbook.ml

import fif.Data

import scala.language.higherKinds

object InfoGainRatioLearning extends FeatureImportance {

  override type Feature = String
  override type Label = Boolean

  override def apply[D[_]: Data](
    data: D[(Seq[String], Boolean)]
  )(implicit fs: FeatureSpace) =
    fs.features.zip(InformationBinaryLabel.gainRatio(data))
}