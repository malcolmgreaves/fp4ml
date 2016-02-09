package mlbigbook.ml

import fif.Data
import mlbigbook.ml.FeatureVectorSupport.FeatureSpace

import scala.language.{ higherKinds, postfixOps }

/**
 * Given a data set, calculates a notion of importance on a per-feature basis.
 */
trait FeatureImportance {
  def apply[D[_]: Data](
    data: D[(Seq[String], Boolean)]
  )(implicit fs: FeatureSpace): Seq[(String, Double)]
}