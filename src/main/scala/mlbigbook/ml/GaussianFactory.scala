package mlbigbook.ml

import mlbigbook.data._

import scala.language.{ higherKinds, implicitConversions }

trait GaussianFactory[@specialized(Float, Double) N] { factory =>

  implicit def num: Integral[N]

  implicit def mops: MathOps[N]

  lazy val one: N = num.one
  lazy val two: N = num.plus(one, one)

  def apply[Label, Feature](
    cardinality: Int,
    data:        Data[(Feature, Label)]
  ): Map[Feature, Gaussian] = ???

  case class Gaussian(
    mean:     N,
    variance: N,
    stddev:   N
  )

  def probabilityOf(gau: Gaussian)(value: N): N = {
    // (1 / ( sqrt ( 2 * pi * stddev^2 ) ) ^ ( e^ (  -(1/2) * (  ( VALUE - mean )  /  stddev  ) )^2  )

    val base = num.quot(
      one,
      mops.sqrt(
        num.times(two, num.times(mops.pi, gau.variance))
      )
    )

    val exponent = {
      val rightPart =
        mops.pow(
          num.quot(
            num.minus(value, gau.mean),
            gau.stddev
          ),
          two
        )
      mops.eTo(
        num.times(mops.negOneHalf, rightPart)
      )
    }

    mops.pow(base, exponent)
  }

  def logProbabilityOf(gau: Gaussian)(value: N): N =
    mops.log(probabilityOf(gau)(value))

}

object GaussianFactory {

  def apply[N: Integral: MathOps]: GaussianFactory[N] = {
    val i = implicitly[Integral[N]]
    val m = implicitly[MathOps[N]]
    new GaussianFactory[N] {
      override implicit val num = i
      override implicit val mops = m
    }
  }

  object Implicits {

    implicit val Double = {
      import MathOps.Implicits._
      GaussianFactory[Double]
    }

    implicit val Float = {
      import MathOps.Implicits._
      GaussianFactory[Float]
    }
  }

}

//package mlbigbook.ml
//
//import mlbigbook.data._
//import mlbigbook.wordcount.GenericCount
//
//import scala.language.{higherKinds, implicitConversions}
//
//
//
//trait GaussianFactory[@specialized(Float, Double) N] { factory =>
//
//  implicit def num: Numeric[N]
//
//  lazy val pi = math.Pi
//  lazy val one = 1.0
//  lazy val two = 2.0
//  lazy val negOneHalf = -0.5
//
//  def apply[Label, Feature](d: Data[(Feature, Label)]): Gaussian
//
//  case class Gaussian(
//    mean: Double,
//    variance: Double,
//    stddev: Double
//  )
//
//  /*** [BEGIN] MATH OPS ***/
//
//  def sqrt[F](v: Vec[F]): Vec[F] =  ???
//
//  def e[F](v: Vec[F]): Vec[F] = ???
//
//  def pow(base: Vec, exponent: Vec): Vec = ???
//
//  /*** [END] MATH OPS ***/
//
//  def probabilityOf(gau: Gaussian)(value: Vec): Vec = {
//   // (1 / ( sqrt ( 2 * pi * stddev^2 ) ) ^ ( e^ (  -(1/2) * (  ( VALUE - mean )  /  stddev  ) )^2  )
//
//   val base =
//    ( one / sqrt(two * pi * gau.variance) )
//
//    val exponent = {
//      val eExponent = {
//
//        val rightPart =
//          pow(( (value - gau.mean) / gau.stddev ), twos)
//
//        negOneHalf * rightPart
//      }
//
//      e(eExponent)
//    }
//
//    pow(base, exponent)
// }
//
// def logProbabilityOf(gau: Gaussian)(value: Vec) =
//  probabilityOf(gau)(value)
//    .map(math.log)
//
//}