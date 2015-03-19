package mlbigbook.ml

import mlbigbook.data.Vector

import scala.util.Random

/** Type represetning a locality sensitive hash function. */
trait LSH extends (Vector => Int)

object LSH {

  /** Constructs nLshFuncs LSH functions. */
  def apply(nLSHFuncs: Int, vectorspaceSize: Int, bandSize: Int)(implicit rand: Random): Seq[LSH] =
    Seq.fill(nLSHFuncs)(apply(vectorspaceSize, bandSize))

  /**
   * Constructs a locality sensitive hash function.
   *
   * The resulting function will be suitable to operate on vectors of cardinality
   * vectorspaceSize. The nBins parameter controls how many bins there are for
   * hashing (the resulting LSH type will output integer values in the range
   * [0, nBins) ). The implicit random number generator is used to select
   * dimensions from the vector space to project vector coordinates into.
   */
  def apply(vectorspaceSize: Int, nBins: Int)(implicit rand: Random): LSH = {
    val selectedDimensions =
      (0 until vectorspaceSize).foldLeft(IndexedSeq.empty[Int])(
        (selected, dimension) =>
          if (rand.nextBoolean)
            selected :+ dimension
          else
            selected
      )

    (v: Vector) => {
      val projectedSum = selectedDimensions.foldLeft(0.0)(
        (sum, dimension) => sum + v.valueAt(dimension)
      )
      projectedSum.round.toInt % nBins
    }
  }

  @inline implicit def fn2lsh(f: Vector => Int): LSH =
    new LSH {
      @inline override def apply(x: Vector): Int = f(x)
    }

}