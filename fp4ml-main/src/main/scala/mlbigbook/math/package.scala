package mlbigbook

package object math {

  implicit val intIsFractional = new Fractional[Int] {
    @inline override def div(x: Int, y: Int) = x / y
    @inline override def toDouble(x: Int) = x.toDouble
    @inline override def plus(x: Int, y: Int) = x + y
    @inline override def toFloat(x: Int) = x.toFloat
    @inline override def toInt(x: Int) = x
    @inline override def negate(x: Int) = -x
    @inline override def fromInt(x: Int) = x
    @inline override def toLong(x: Int) = x.toLong
    @inline override def times(x: Int, y: Int) = x * y
    @inline override def minus(x: Int, y: Int) = x - y
    @inline override def compare(x: Int, y: Int) = x - y
  }

  implicit val longIsFractional = new Fractional[Long] {
    @inline override def div(x: Long, y: Long) = x / y
    @inline override def toDouble(x: Long) = x.toDouble
    @inline override def plus(x: Long, y: Long) = x + y
    @inline override def toFloat(x: Long) = x.toFloat
    @inline override def toInt(x: Long) = x.toInt
    @inline override def negate(x: Long) = -x
    @inline override def fromInt(x: Int) = x.toLong
    @inline override def toLong(x: Long) = x
    @inline override def times(x: Long, y: Long) = x * y
    @inline override def minus(x: Long, y: Long) = x - y
    @inline override def compare(x: Long, y: Long) = (x - y).toInt
  }

}