package mlbigbook.data

import java.util.Random

trait DenseVectorBuilder {

  val cardinality: Int

  /**
   * Mutable operation.
   *
   * Assigns
   */
  def update(index: Int, value: Double): Unit

  /**
   * Mutable operation.
   *
   * For each non-zero value in the input vector v, it adds this value
   * to the current value at this vector builder's appropriate index.
   */
  def add(v: Vector): Unit

  /**
   * Mutable operation.
   *
   * Divides each element by the input value. If the value is 0, then no
   * operation is performed as division by zero is undefined.
   */
  def normalize(value: Double): Unit

  /**
   * Create a Vector instance using the current contents of this builder.
   *
   * The evaluated Vector instance is free from side effects. Namely, further
   * updated to this builder will not affect the evalauted Vector's contents.
   */
  def create: Vector

}

object DenseVectorBuilder {

  def apply(knownCardinality: Int): DenseVectorBuilder =

    new DenseVectorBuilder {

      private val denseValues = new Array[Double](knownCardinality)

      override val cardinality =
        knownCardinality

      @inline override def add(v: Vector): Unit =
        if (v.cardinality == knownCardinality) {
          v.nonZeros.foreach({
            case (index, value) =>
              denseValues(index) += value
          })
        }

      @inline override def update(index: Int, value: Double): Unit =
        if (index >= 0 && index < knownCardinality) {
          denseValues.update(index, value)
        }

      @inline def normalize(value: Double): Unit =
        if (value != 0.0) {
          var i = 0
          while (i < knownCardinality) {
            denseValues(i) /= value
            i += 1
          }
        }

      override def create: Vector =
        DenseVector({
          val copy = new Array[Double](knownCardinality)
          System.arraycopy(denseValues, 0, copy, 0, knownCardinality)
          copy
        })

    }

}

object DenseVector {

  def apply(frozenDenseValues: Array[Double]): Vector =

    new Vector {

      override val cardinality =
        frozenDenseValues.length

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

  def mkRandom(knownCardinality: Int)(implicit rand: Random): Vector = {
    val uniformRandomValues = new Array[Double](knownCardinality)

    var i = 0
    while (i < knownCardinality) {
      uniformRandomValues(i) = rand.nextDouble()
      i += 1
    }

    DenseVector(uniformRandomValues)
  }

}

