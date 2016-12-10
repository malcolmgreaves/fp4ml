package mlbigbook.math

import scala.reflect.ClassTag

/**
 * Typeclass supporting conversions between primitive types, with the
 * constraint that the primitive has Numeric evidence.
 */
sealed abstract class NumericConversion[@specialized N] {

  def fromInt(i: Int): N

  def fromLong(l: Long): N

  def fromDouble(d: Double): N

  def fromByte(b: Byte): N

  def fromShort(s: Short): N

  def fromFloat(f: Float): N

  implicit def ct: ClassTag[N]
}

object NumericConversion {

  def apply[N: NumericConversion]: NumericConversion[N] =
    implicitly[NumericConversion[N]]

  /**
   * Implicit NumericConversion instances for every primitive numeric type:
   * float, long, double, int, short, byte
   */
  object Implicits {

    implicit case object FloatC extends NumericConversion[Float] {
      override implicit val ct: ClassTag[Float] = ClassTag(classOf[Float])
      override def fromInt(l: Int) = l.toFloat
      override def fromLong(l: Long) = l.toFloat
      override def fromShort(s: Short) = s.toFloat
      override def fromByte(b: Byte) = b.toFloat
      override def fromDouble(d: Double) = d.toFloat
      override def fromFloat(f: Float) = f
    }

    implicit case object LongC extends NumericConversion[Long] {
      override implicit val ct: ClassTag[Long] = ClassTag(classOf[Long])
      override def fromInt(l: Int) = l.toLong
      override def fromLong(l: Long) = l
      override def fromShort(s: Short) = s.toLong
      override def fromByte(b: Byte) = b.toLong
      override def fromDouble(d: Double) = d.toLong
      override def fromFloat(f: Float) = f.toLong
    }

    implicit case object DoubleC extends NumericConversion[Double] {
      override implicit val ct: ClassTag[Double] = ClassTag(classOf[Double])
      override def fromInt(l: Int) = l.toDouble
      override def fromLong(l: Long): Double = l.toDouble
      override def fromShort(s: Short): Double = s.toDouble
      override def fromByte(b: Byte): Double = b.toDouble
      override def fromDouble(d: Double): Double = d
      override def fromFloat(f: Float): Double = f.toDouble
    }

    implicit case object IntC extends NumericConversion[Int] {
      override implicit val ct: ClassTag[Int] = ClassTag(classOf[Int])
      override def fromInt(l: Int) = l.toInt
      override def fromLong(l: Long) = l.toInt
      override def fromShort(s: Short) = s.toInt
      override def fromByte(b: Byte) = b.toInt
      override def fromDouble(d: Double) = d.toInt
      override def fromFloat(f: Float) = f.toInt
    }

    implicit case object ShortC extends NumericConversion[Short] {
      override implicit val ct: ClassTag[Short] = ClassTag(classOf[Short])
      override def fromInt(l: Int) = l.toShort
      override def fromLong(l: Long) = l.toShort
      override def fromShort(s: Short) = s
      override def fromByte(b: Byte) = b.toShort
      override def fromDouble(d: Double) = d.toShort
      override def fromFloat(f: Float) = f.toShort
    }

    implicit case object ByteC extends NumericConversion[Byte] {
      override implicit val ct: ClassTag[Byte] = ClassTag(classOf[Byte])
      override def fromInt(l: Int) = l.toByte
      override def fromLong(l: Long) = l.toByte
      override def fromShort(s: Short) = s.toByte
      override def fromByte(b: Byte) = b
      override def fromDouble(d: Double) = d.toByte
      override def fromFloat(f: Float) = f.toByte
    }
  }
}
