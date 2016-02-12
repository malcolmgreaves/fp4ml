package mlbigbook.ml

import fif.Data
import mlbigbook.data._
import mlbigbook.math.{ VectorOpsT, NumericX }

import scala.language.{ higherKinds, implicitConversions }

case class Gaussian[N: Numeric](mean: N, variance: N, stddev: N)

object Gaussian {

  def apply[D[_]: Data, N: Numeric: MathOps, V[_] <: Vector](
    data: D[V[N]]
  )(
    implicit
    fs:   FeatureSpace,
    vops: VectorOpsT[N, V]
  ): Seq[Gaussian[N]] = ???

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