package mlbigbook.ml

import mlbigbook.data._

import scala.language.{ higherKinds, implicitConversions }

object GaussianFactory {

  def apply[N: Fractional: MathOps]: GaussianFactory[N] = {
    val i = implicitly[Fractional[N]]
    val m = implicitly[MathOps[N]]
    new GaussianFactory[N] {
      override implicit val num = i
      override implicit val mops = m
    }
  }

  object Implicits {

    import MathOps.Implicits._

    implicit val DoubleGf = GaussianFactory[Double]
    implicit val FloatFg = GaussianFactory[Float]
  }
}

trait GaussianFactory[@specialized(Float, Double) N] { factory =>

  implicit def num: Numeric[N]

  implicit def mops: MathOps[N]

  lazy val one: N = num.one
  lazy val two: N = num.plus(one, one)

  def apply[Label, Feature](
    cardinality: Int,
    data:        DataClass[Feature.Vector[Feature, N]]
  ): Map[Feature, Gaussian] = ???

  case class Gaussian(mean: N, variance: N, stddev: N)

  def probabilityOf(gau: Gaussian)(value: N): N = {
    // (1 / ( sqrt ( 2 * pi * stddev^2 ) ) ^ ( e^ (  -(1/2) * (  ( VALUE - mean )  /  stddev  ) )^2  )

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

  def logProbabilityOf(gau: Gaussian)(value: N): N =
    mops.log(probabilityOf(gau)(value))

}