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