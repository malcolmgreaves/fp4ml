package mlbigbook.math

/**
 * Typeclass supporting conversions between primitive types, with the
 * constraint that the primitive has Numeric evidence.
 */
sealed abstract class NumericX[@specialized N: Numeric] {

  private[this] val numeric = implicitly[Numeric[N]]

  def div(a: N, b: N): N

  final def plus(x: N, y: N): N =
    numeric.plus(x, y)

  final def toDouble(x: N): Double =
    numeric.toDouble(x)

  final def toFloat(x: N): Float =
    numeric.toFloat(x)

  final def toInt(x: N): Int =
    numeric.toInt(x)

  final def negate(x: N): N =
    numeric.negate(x)

  final def fromInt(x: Int): N =
    numeric.fromInt(x)

  final def toLong(x: N): Long =
    numeric.toLong(x)

  final def times(x: N, y: N): N =
    numeric.times(x, y)

  final def minus(x: N, y: N): N =
    numeric.minus(x, y)

  final def compare(x: N, y: N): Int =
    numeric.compare(x, y)
}

object NumericX {

  /**
   * Implicit NumericX instances for every primitive numeric type:
   * float, long, double, int, short, byte
   */
  object Implicits {

    implicit case object FloatX extends NumericX[Float] {
      override def div(a: Float, b: Float): Float = a / b
    }

    implicit case object LongX extends NumericX[Long] {
      override def div(a: Long, b: Long): Long = a / b
    }

    implicit case object DoubleX extends NumericX[Double] {
      override def div(a: Double, b: Double): Double = a / b
    }

    implicit case object IntX extends NumericX[Int] {
      override def div(a: Int, b: Int): Int = a / b
    }

    implicit case object ShortX extends NumericX[Short] {
      override def div(a: Short, b: Short): Short = (a / b).toByte
    }

    implicit case object ByteX extends NumericX[Byte] {
      override def div(a: Byte, b: Byte): Byte = (a / b).toByte
    }
  }
}
