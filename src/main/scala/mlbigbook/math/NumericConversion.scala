package mlbigbook.util

/**
 * Typeclass supporting conversions between primitive types, with the
 * constraint that the primitive has Numeric evidence.
 */
sealed abstract class NumericConversion[@specialized N: Numeric] {

  val numeric = implicitly[Numeric[N]]

  final def fromInt(i: Int): N =
    numeric.fromInt(i)

  def fromLong(l: Long): N

  def fromDouble(d: Double): N

  def fromByte(b: Byte): N

  def fromShort(s: Short): N

  def fromFloat(f: Float): N
}

object NumericConversion {

  /**
   * Implicit NumericConversion instances for every primitive numeric type:
   * float, long, double, int, short, byte
   */
  object Implicits {

    implicit case object FloatC extends NumericConversion[Float] {
      override def fromLong(l: Long): Float = l.toFloat
      override def fromShort(s: Short): Float = s.toFloat
      override def fromByte(b: Byte): Float = b.toFloat
      override def fromDouble(d: Double): Float = d.toFloat
      override def fromFloat(f: Float): Float = f
    }

    implicit case object LongC extends NumericConversion[Long] {
      override def fromLong(l: Long): Long = l
      override def fromShort(s: Short): Long = s.toLong
      override def fromByte(b: Byte): Long = b.toLong
      override def fromDouble(d: Double): Long = d.toLong
      override def fromFloat(f: Float): Long = f.toLong
    }

    implicit case object DoubleC extends NumericConversion[Double] {
      override def fromLong(l: Long): Double = l.toDouble
      override def fromShort(s: Short): Double = s.toDouble
      override def fromByte(b: Byte): Double = b.toDouble
      override def fromDouble(d: Double): Double = d
      override def fromFloat(f: Float): Double = f.toDouble
    }

    implicit case object IntC extends NumericConversion[Int] {
      override def fromLong(l: Long): Int = l.toInt
      override def fromShort(s: Short): Int = s.toInt
      override def fromByte(b: Byte): Int = b.toInt
      override def fromDouble(d: Double): Int = d.toInt
      override def fromFloat(f: Float): Int = f.toInt
    }

    implicit case object ShortC extends NumericConversion[Short] {
      override def fromLong(l: Long): Short = l.toShort
      override def fromShort(s: Short): Short = s
      override def fromByte(b: Byte): Short = b.toShort
      override def fromDouble(d: Double): Short = d.toShort
      override def fromFloat(f: Float): Short = f.toShort
    }

    implicit case object ByteC extends NumericConversion[Byte] {
      override def fromLong(l: Long): Byte = l.toByte
      override def fromShort(s: Short): Byte = s.toByte
      override def fromByte(b: Byte): Byte = b
      override def fromDouble(d: Double): Byte = d.toByte
      override def fromFloat(f: Float): Byte = f.toByte
    }
  }
}
