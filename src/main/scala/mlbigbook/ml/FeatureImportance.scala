package mlbigbook.ml

import fif.Data
import mlbigbook.ml.FeatureVectorSupport.FeatureSpace

import scala.language.{ higherKinds, postfixOps }

trait FeatureImportance {
  def apply[D[_]: Data](
    data: D[(Seq[String], Boolean)]
  )(
    implicit
    fs: FeatureSpace
  ): Seq[(String, Double)]
}