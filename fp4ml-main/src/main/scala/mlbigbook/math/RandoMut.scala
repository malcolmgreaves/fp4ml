package mlbigbook.math

import scala.util.Random

abstract class RandoMut[N: Fractional] {
  def next(): N
}

object RandoMut {

  def newSeedPerCall[N: Fractional: NumericConversion]: () => RandoMut[N] =
    () => fromSeed(Random.nextLong())

  def fromSeed[N: Fractional: NumericConversion](seed: Long): RandoMut[N] = {
    val r = new Random(seed)
    new RandoMut[N] {
      override def next() = NumericConversion[N].fromDouble(r.nextDouble())
    }
  }

}
