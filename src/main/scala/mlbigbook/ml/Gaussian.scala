package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import Data.ops._
import mlbigbook.math.{ OnlineMeanVariance, NumericConversion, VectorOpsT }

import scala.language.{ higherKinds, implicitConversions }

case class Gaussian[N: Numeric](mean: N, variance: N, stddev: N)

object Gaussian {

  def apply[D[_]: Data, N: NumericConversion: MathOps, V[_] <: Vector[_]](
    data: D[V[N]]
  )(
    implicit
    fs:   FeatureSpace,
    vops: VectorOpsT[N, V]
  ): Seq[Gaussian[N]] =

    if (data isEmpty)
      Seq.empty[Gaussian[N]]

    else {
      implicit val _ = NumericConversion[N].numeric

      val statsForEachFeature = OnlineMeanVariance.batch(data)

      (0 until fs.size).map { fIndex =>

        val variance =
          VectorOpsT[N, V].valueAt(statsForEachFeature.variance)(fIndex)

        Gaussian(
          mean = VectorOpsT[N, V].valueAt(statsForEachFeature.mean)(fIndex),
          variance = variance,
          stddev = MathOps[N].sqrt(variance)
        )
      }
    }

  def probabilityOf[N: Numeric: MathOps](gau: Gaussian[N])(value: N): N = {
    // (1 / ( sqrt ( 2 * pi * stddev^2 ) ) ^ ( e^ (  -(1/2) * (  ( VALUE - mean )  /  stddev  ) )^2  )

    val mops = MathOps[N]
    val num = implicitly[Numeric[N]]
    val one = num.one
    val two = num.plus(one, one)

    val base = mops.div(
      one,
      mops.sqrt(
        num.times(two, num.times(mops.pi, gau.variance))
      )
    )

    val ePart = {
      val rightPart =
        mops.pow(
          mops.div(
            num.minus(value, gau.mean),
            gau.stddev
          ),
          two
        )
      mops.eTo(
        num.times(mops.negOneHalf, rightPart)
      )
    }

    num.times(base, ePart)
  }

  def logProbabilityOf[N: Numeric: MathOps](gau: Gaussian[N])(value: N): N =
    MathOps[N].log(probabilityOf(gau)(value))

}