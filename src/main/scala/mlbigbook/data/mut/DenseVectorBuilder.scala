package mlbigbook.data.mut

import java.util.Random

import mlbigbook.data.OLD_Vector

/**
 * WARNING: DenseVectorBuilder is a highly mutable class!
 *
 * Allows for efficient mutation a fixed-size array with a
 * restricted interface and a safe way to create immutable Vector instances
 * using the builder's contents.
 *
 * The field cardinality indicates the size of the vector space size that
 * this class is building in.
 */
class DenseVectorBuilder(val cardinality: Int) {

  // internal storage of the builder's values
  private val denseValues = new Array[Double](cardinality)

  /**
   * Immutable operation.
   *
   * Obtains the current value of the builder at the input index.
   *
   * Evaluates to 0.0 if the index is out of bounds.
   */
  @inline def get(index: Int): Double =
    if (index >= 0 && index < cardinality)
      denseValues(index)
    else
      0.0

  /**
   * Mutable operation.
   *
   * For each non-zero value in the input vector v, it adds this value
   * to the current value at this vector builder's appropriate index.
   *
   * If the builder's and input vector's cardinality differs, then the
   * add call is a no-op.
   */
  def add(v: OLD_Vector): Unit =
    if (v.cardinality == cardinality) {
      v.nonZeros.foreach({
        case (index, value) =>
          denseValues(index) += value
      })
    }

  /**
   * Mutable operation.
   *
   * Adds each value of the input DenseVectorBuilder to this internal storage.
   *
   * If the builder's and input vector's cardinality differs, then the
   * add call is a no-op.
   */
  def add(dvb: DenseVectorBuilder): Unit =
    if (dvb.cardinality == cardinality) {
      var i = 0
      while (i < cardinality) {
        denseValues(i) += dvb.denseValues(i)
        i += 1
      }
    }

  /**
   * Mutable operation.
   *
   * Assigns the input value at the builder's index.
   *
   * If the index is out of bounds, then the call to update is a no-op.
   */
  @inline def update(index: Int, value: Double): Unit =
    if (index >= 0 && index < cardinality) {
      denseValues.update(index, value)
    }

  /**
   * Mutable operation.
   *
   * Divides each element by the input value.
   *
   * If the value is 0, then no operation is performed as division by
   * zero is undefined.
   */
  def normalize(value: Double): Unit =
    if (value != 0.0) {
      var i = 0
      while (i < cardinality) {
        denseValues(i) /= value
        i += 1
      }
    }

  /**
   * Immutable operation.
   *
   * Creates a Vector instance using the current contents of this builder.
   *
   * If copyValues=true, then the evaluated Vector instance is free from side effects.
   * Namely, further updated to this builder will not affect the evaluated Vector's contents.
   * This is the prefered invocation. Only call with copyValues=false if you can guarentee
   * that the builder will be discarded after calling create.
   */
  def create(copyValues: Boolean = true): OLD_Vector =
    DenseVector(denseValues, copyValues)

}

object DenseVector {

  /**
   * Creates a Vector instance that is backed up by the mutable array.
   *
   * It is ABSOLUTELY CRITICAL that these frozenValues are NOT MUTATED
   * after calling apply. Otherwise, it will break the Vector type contract.
   *
   * When in doubt, set copyValues to true (it defaults to this behavior).
   *
   * The input Array[Double] frozenValues is assumed to have each and
   * every value of the vector. So frozenValues(i) == the value of the
   * vector's ith dimension. The first valid dimension value is 0.
   */
  def apply(frozenValues: Array[Double], copyValues: Boolean = true): OLD_Vector =

    new OLD_Vector {

      override val cardinality =
        frozenValues.length

      private val frozenDenseValues =
        if (copyValues) {
          val copy = new Array[Double](cardinality)
          System.arraycopy(frozenValues, 0, copy, 0, cardinality)
          copy
        } else {
          frozenValues
        }

      @inline override def valueAt(dimension: Int): Double =
        if (dimension >= 0 && dimension < cardinality)
          frozenDenseValues(dimension)
        else
          0.0

      override lazy val nonZeros: Traversable[(Int, Double)] =
        frozenDenseValues
          .zipWithIndex
          .filter(x => x._1 > 0.0 || x._2 < 0.0)
          .map(_.swap)
          .toTraversable

    }

  /** Creates a Vector of knownCardinality size; dimensions drawn uniformly from [0,1]. */
  def mkRandom(knownCardinality: Int)(implicit rand: Random): OLD_Vector = {
    val uniformRandomValues = new Array[Double](knownCardinality)

    var i = 0
    while (i < knownCardinality) {
      uniformRandomValues(i) = rand.nextDouble()
      i += 1
    }

    DenseVector(uniformRandomValues)
  }

}

