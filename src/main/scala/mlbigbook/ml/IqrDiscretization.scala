package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import fif.Data.ops._
import mlbigbook.math.VectorOpsT
import mlbigbook.ml.FeatureVectorSupport.FeatureSpace

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object IqrDiscretization extends Discretization {

  val discretizedValues =
    Seq("below_min", "min_q1", "q1_median", "median_q2", "q2_max", "above_or_equal_to_max")

  def apply[D[_]: Data, V[_] <: Vector[_], N: Numeric: ClassTag](
    data:    D[V[N]],
    headers: Seq[String]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): (D[Seq[String]], Seq[Seq[String]]) = {

    val fiveNumberSummaries = Iqr(data)
    if (fiveNumberSummaries isEmpty)
      (data.map(_ => Seq.empty[String]), Seq.empty[Seq[String]])

    else {

      val newDiscreteValuesPerFeat =
        headers.map { featureName =>
          discretizedValues.map { dv => s"$dv-$featureName" }
        }

      val discretizedData: D[Seq[String]] = {
        val lessThan = implicitly[Numeric[N]].lt _
        data
          .map { vector =>
            vops.toSeq(vector)
              .zip(fiveNumberSummaries)
              .map {
                case (value, fns) =>
                  // ordering of if statements below is _important_ !!
                  if (lessThan(value, fns.min))
                    discretizedValues.head

                  else if (lessThan(value, fns.q1))
                    discretizedValues(1)

                  else if (lessThan(value, fns.median))
                    discretizedValues(2)

                  else if (lessThan(value, fns.q2))
                    discretizedValues(3)

                  else if (lessThan(value, fns.max))
                    discretizedValues(4)

                  else
                    discretizedValues.last
              }
          }
      }

      (discretizedData, newDiscreteValuesPerFeat)
    }
  }

}