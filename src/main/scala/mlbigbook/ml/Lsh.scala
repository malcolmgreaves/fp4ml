package mlbigbook.ml

import mlbigbook.data.{ DataClass, OLD_Vector }

import scala.util.Random

/** Type represetning a locality sensitive hash function. */
trait Lsh extends (OLD_Vector => Int)

case class LshIn(cardinality: Int, nBins: Int)

object Lsh {

  /** Constructs nLshFuncs LSH functions. */
  def apply(nLshFuncs: Int)(c: LshIn)(implicit rand: Random): Seq[Lsh] =
    Seq.fill(nLshFuncs)(apply(c))

  /**
   * Constructs a locality sensitive hash function.
   *
   * The resulting function will be suitable to operate on vectors of cardinality
   * vectorspaceSize. The nBins parameter controls how many bins there are for
   * hashing (the resulting LSH type will output integer values in the range
   * [0, nBins) ). The implicit random number generator is used to select
   * dimensions from the vector space to project vector coordinates into.
   */
  def apply(c: LshIn)(implicit rand: Random): Lsh = {
    val selectedDimensions =
      (0 until c.cardinality).foldLeft(IndexedSeq.empty[Int])(
        (selected, dimension) =>
          if (rand.nextBoolean)
            selected :+ dimension
          else
            selected
      )

    (v: OLD_Vector) => {
      val projectedSum = selectedDimensions.foldLeft(0.0)(
        (sum, dimension) => sum + v.valueAt(dimension)
      )
      projectedSum.round.toInt % c.nBins
    }
  }

  @inline implicit def fn2lsh(f: OLD_Vector => Int): Lsh =
    new Lsh {
      @inline override def apply(x: OLD_Vector): Int = f(x)
    }

}