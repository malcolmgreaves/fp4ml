package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import fif.Data.ops._
import mlbigbook.math.{ NumericConversion, VectorOpsT }
import spire.syntax.cfor._

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object GaussianDiscretization {

  trait Rule {
    def apply[N: NumericConversion](g: Gaussian[N])(value: N): String
    def discretizedValues: Seq[String]
  }

  import Discretization._

  def apply[D[_]: Data, V[_] <: Vector[_], N: NumericConversion: MathOps: ClassTag](
    data:   D[V[N]],
    ruling: Rule
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): (D[DiscretizedVector], FeatureSpace) = {

    val gaussians = Gaussian.estimate(data)

    if (gaussians isEmpty)
      (data.map(_ => Seq.empty[String]), FeatureSpace.empty)

    else {

      val readyRulings = gaussians.map { g => ruling[N](g) _ }

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
        Discretization.newCategoricalFs(ruling.discretizedValues)
      )
    }
  }

  def apply(ruling: Rule): Discretization =
    new Discretization {
      override def apply[D[_]: Data, V[_] <: Vector[_], N: NumericConversion: ClassTag](
        data: D[V[N]]
      )(
        implicit
        vops: VectorOpsT[N, V],
        fs:   FeatureSpace
      ): (D[DiscretizedVector], FeatureSpace) = ??? //GaussianDiscretization(data, ruling)
    }

}