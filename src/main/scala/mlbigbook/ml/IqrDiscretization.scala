package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import fif.Data.ops._
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object IqrDiscretization extends Discretization {

  val below_min = "below_min"
  val min_q1 = "between_min_inclusive_and_q1_exclusive"
  val q1_median = "between_q1_inclusive_and_median_exclusive"
  val median_q2 = "between_median_inclusive_and_q2_exclusive"
  val q2_max = "between_q2_inclusive_and_max_exclusive"
  val above_or_equal_to_max = "above_or_equal_to_max"

  val discretizedValues = Seq(
    below_min, min_q1, q1_median, median_q2, q2_max, above_or_equal_to_max
  )

  override def apply[D[_]: Data, V[_] <: Vector[_], N: NumericConversion: ClassTag](
    data: D[V[N]]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ) = {

    val fiveNumberSummaries = {
      implicit val _ = NumericConversion[N].numeric
      InterQuartileRange(data)
    }

    if (fiveNumberSummaries isEmpty)
      (data.map(_ => Seq.empty[String]), FeatureSpace.empty)

    else {

      val newDiscreteValuesPerFeat =
        fs.features
          .map { featureName =>
            (
              featureName,
              discretizedValues.map { dv => s"$dv-$featureName" }
            )
          }
          .toMap

      val discretizedData: D[Seq[String]] = {
        val lessThan = NumericConversion[N].numeric.lt _
        data.map { vector =>
          vops.toSeq(vector)
            .zip(fiveNumberSummaries)
            .map {
              case (value, fns) =>
                // ordering of if statements below is _important_ !!
                if (lessThan(value, fns.min))
                  below_min

                else if (lessThan(value, fns.q1))
                  min_q1

                else if (lessThan(value, fns.median))
                  q1_median

                else if (lessThan(value, fns.q2))
                  median_q2

                else if (lessThan(value, fns.max))
                  q2_max

                else
                  above_or_equal_to_max
            }
        }
      }

      (
        discretizedData,
        FeatureSpace.allCategorical(fs.features, newDiscreteValuesPerFeat)
      )
    }
  }

}