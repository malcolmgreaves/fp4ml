package mlbigbook.newthings

import mlbigbook.math.NumericConversion

import scala.util.Random

abstract class RandoMut[N: Numeric] {
  def next(): N
}

object RandoMut {

  def newSeedPerCall[N: NumericConversion]: () => RandoMut[N] =
    () => fromSeed(Random.nextLong())

  def fromSeed[N: NumericConversion](seed: Long): RandoMut[N] = {
    val r = new Random(seed)
    implicit val _ = NumericConversion[N].numeric
    new RandoMut[N] {
      override def next() = NumericConversion[N].fromDouble(r.nextDouble())
    }
  }

}