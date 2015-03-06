package mlbigbook.lsh

import scala.util.Random
import mlbigbook.wordcount.Vector

object LSH {

  /** Type definition for a locality sensitive hash function. */
  type Type = Vector => Int

  /**
   * Constructs nLSHFuncs LSH functions, each of which are appropriate for vectors
   * of size vectorspaceSize.
   */
  def create(nLSHFuncs: Int, vectorspaceSize: Int, bandSize: Int)(implicit rand: Random): Seq[LSH.Type] =
    Seq.fill(nLSHFuncs)(create(vectorspaceSize, bandSize))

  /**
   * Constructs a locality sensistive hash function that is appropriate for a vector space
   * of size vectorspaceSize. The implicit random number generator is used to select dimensions
   * from the vector space to project vector coordinates into. The resulting function can then
   * be used to project coordinates from vectors, sum these projected coordinates, and then
   * compute the modulo of this summation with respect to the band size (bandSize).
   */
  def create(vectorspaceSize: Int, bandSize: Int)(implicit rand: Random): Type = {
    val selectedDimensions = (0 until vectorspaceSize).foldLeft(IndexedSeq.empty[Int])(
      (selected, dimension) => 
        if(rand.nextBoolean)
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

}
