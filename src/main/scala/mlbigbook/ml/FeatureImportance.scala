package mlbigbook.ml

import fif.Data

import scala.language.higherKinds

/**
 * Given a data set, calculates a notion of importance on a per-feature basis.
 */
trait FeatureImportance {

  type Feature
  type Label

  def apply[D[_]: Data](
    data: D[(Seq[Feature], Label)]
  )(implicit fs: FeatureSpace): Seq[(Feature, Double)]

}

object FeatureImportance {

  type Type[F, L] = FeatureImportance {
    type Feature = F
    type Label = L
  }
}