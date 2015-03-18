package mlbigbook.lsh

import scala.util.Random
import mlbigbook.data.Vector

/** Type represetning a locality sensitive hash function. */
trait LSH extends (Vector => Int)

object LSH {

  /**
   * Constructs nLSHFuncs LSH functions, each of which are appropriate for vectors
   * of size vectorspaceSize.
   */
  def apply(nLSHFuncs: Int, vectorspaceSize: Int, bandSize: Int)(implicit rand: Random): Seq[LSH] =
    Seq.fill(nLSHFuncs)(apply(vectorspaceSize, bandSize))

  /**
   * Constructs a locality sensistive hash function that is appropriate for a vector space
   * of size vectorspaceSize. The implicit random number generator is used to select dimensions
   * from the vector space to project vector coordinates into. The resulting function can then
   * be used to project coordinates from vectors, sum these projected coordinates, and then
   * compute the modulo of this summation with respect to the band size (bandSize).
   */
  def apply(vectorspaceSize: Int, bandSize: Int)(implicit rand: Random): LSH = {
    val selectedDimensions = (0 until vectorspaceSize).foldLeft(IndexedSeq.empty[Int])(
      (selected, dimension) =>
        if (rand.nextBoolean)
          selected :+ dimension
        else
          selected
    )

    (v: Vector) => {
      val projectedSum = selectedDimensions.foldLeft(0.0)(
        (accum, dimension) => accum + v.valueAt(dimension)
      )
      projectedSum.round.toInt % bandSize
    }
  }

  implicit def fn2lsh(f: Vector => Int): LSH =
    new LSH {
      override def apply(x: Vector): Int = f(x)
    }

}