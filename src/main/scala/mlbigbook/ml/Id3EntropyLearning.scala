package mlbigbook.ml

import fif.{ Data, TravData }
import mlbigbook.ml.FeatureVectorSupport.FeatureSpace

import scala.language.{ higherKinds, postfixOps }

object Id3EntropyLearning extends FeatureImportance {

  import Data.ops._

  override def apply[D[_]: Data](
    data: D[(Seq[String], Boolean)]
  )(
    implicit
    fs: FeatureSpace
  ) =
    InformationSimpleFv.entropyCategorical(
      data.map {
        case (categoricalFeatures, _) => categoricalFeatures
      }
    ).toSeq
}