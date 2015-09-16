package mlbigbook.ml

import scala.language.implicitConversions

trait MathOps[N] {
  def pi: N
  def negOneHalf: N
  def sqrt(v: N): N
  def eTo(v: N): N
  def pow(base: N, exponent: N): N
  def log(v: N): N
}

object MathOps {

  object Implicits {

    implicit val Double = new MathOps[Double] {

      override def sqrt(v: Double): Double =
        math.sqrt(v)

      override def log(v: Double): Double =
        math.log(v)

      override val negOneHalf: Double =
        -0.5

      override def pow(base: Double, exponent: Double): Double =
        math.pow(base, exponent)

      override val pi: Double =
        math.Pi

      override def eTo(v: Double): Double =
        math.exp(v)
    }

    implicit val Float = new MathOps[Float] {

      override def sqrt(v: Float): Float =
        math.sqrt(v.toDouble).toFloat

      override def log(v: Float): Float =
        math.log(v.toDouble).toFloat

      override val negOneHalf: Float =
        -0.5f

      override def pow(base: Float, exponent: Float): Float =
        math.pow(base.toDouble, exponent.toDouble).toFloat

      override val pi: Float =
        math.Pi.toFloat

      override def eTo(v: Float): Float =
        math.exp(v.toDouble).toFloat
    }
  }

}