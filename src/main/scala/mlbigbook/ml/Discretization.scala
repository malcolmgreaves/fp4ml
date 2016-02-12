package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }
import spire.syntax.cfor._

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object Discretization {

  type DiscretizedVector = Seq[String]

  def newCategoricalFs(
    rules: Seq[Rule[_]]
  )(implicit fs: FeatureSpace): FeatureSpace =

    FeatureSpace.allCategorical(
      fs.features,
      fs.features.zip(rules)
      .map {
        case (feature, rule) =>
          (
            feature,
            rule.discretizedValueBases.map { dv => s"$dv--$feature" }
          )
      }
      .toMap
    )

  def apply[D[_]: Data, V[_], N: NumericConversion: ClassTag](
    data: D[V[N]],
    rp:   RuleProducer[N]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): (D[DiscretizedVector], FeatureSpace) = {
    import Data.ops._

    if (data isEmpty)
      (data.map(_ => Seq.empty[String]), FeatureSpace.empty)

    else {

      val readyRulings = rp(data)

      val discretizedData = {
        implicit val _ = NumericConversion[N].numeric
        data.map { vector =>
          val valAt = vops.valueAt(vector) _
          //
          // MUTATION WARNING
          //
          // We allocate a local array and loop over it to apply the ruling for
          // each feature. Outside this function, we cannot view this side
          // effect as we evaluate to a Seq[String] (converting the Array we're
          // building).
          //
          val res = new Array[String](fs.size)
          cfor(0)(_ < fs.size, _ + 1) { fIndex =>
            res(fIndex) = readyRulings(fIndex)(valAt(fIndex))
          }
          res.toSeq
          // equivalent to the following side-effect free code:
          // VectorOpsT[N,V].toSeq(vector).zip(readyRulings).map { case (value, rule) => rule(value) }
        }
      }

      (
        discretizedData,
        Discretization.newCategoricalFs(readyRulings)
      )
    }
  }

}