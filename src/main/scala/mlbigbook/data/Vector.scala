/*
 * Abstraction for linear algebra vector. Also contains some common operations as methods.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import mlbigbook.ml.Distance

import scala.annotation.tailrec
import scala.collection.mutable

/** Type defines operations on data that behaves as a dense numerical vector. */
trait Vector {

  /** The size of the feature space that this vector is based on. */
  val cardinality: Int

  /** The vector's value at the given dimension. */
  def valueAt(dimension: Int): Double

  /**
   * zip two vectors together
   *
   * The resulting traversable will yield a pair of values from each zipped vector
   * along with their respective index. For each one of these triples, at least
   * one value is non-zero.
   */
  final def zip(v: Vector): Traversable[(Int, Double, Double)] = {
    assert(cardinality == v.cardinality)

    val buf = mutable.UnrolledBuffer.empty[(Int, Double, Double)]

    @tailrec @inline def zipper(aNZ: Traversable[(Int, Double)], bNZ: Traversable[(Int, Double)]): Unit =
      aNZ.headOption match {

        case Some((aIndex, aHead)) => bNZ.headOption match {

          case Some((bIndex, bHead)) =>
            if (aIndex < bIndex) {
              if (aIndex != 0.0)
                buf.append((aIndex, aHead, 0.0))
              // else don't append because they're both zero!
              zipper(aNZ.drop(1), bNZ)

            } else if (bIndex < aIndex) {
              if (bIndex != 0.0)
                buf.append((bIndex, 0.0, bHead))
              // else don't append because they're both zero!
              zipper(aNZ, bNZ.drop(1))

            } else {

              if (aIndex != 0.0 && bIndex != 0.0)
                buf.append((aIndex, aHead, bHead))
              else if (aIndex != 0.0)
                buf.append((aIndex, aHead, 0.0))
              else if (bIndex != 0.0)
                buf.append((aIndex, 0.0, bHead))
              // else don't append because they're both zero!
              zipper(aNZ.drop(1), bNZ.drop(1))
            }

          case None =>
            if (aIndex != 0.0)
              buf.append((aIndex, aHead, 0.0))
            // else don't append because they're both zero!
            zipper(aNZ.drop(1), bNZ)
        }

        case None => bNZ.headOption match {

          case Some((bIndex, bHead)) =>
            if (bIndex != 0.0)
              buf.append((bIndex, 0.0, bHead))
            // else don't append because they're both zero!
            zipper(aNZ, bNZ.drop(1))

          case None =>
            () // no more elements left to process, we're done
        }
      }

    zipper(nonZeros, v.nonZeros)

    buf.toTraversable
  }

  /** Lists the non-zero indicies and accompanying values for the vector. */
  val nonZeros: Traversable[(Int, Double)]
}

/** Collection of vector operations and type definitions. */
object Vector {

  /**
   * Compute the dot-product of two vectors.
   *
   * Fails with assertion error if the vector cardinalities do not match up.
   */
  def dotProduct(v1: Vector, v2: Vector): Double = {
    v1.zip(v2).foldLeft(0.0)({
      case (dpSum, (_, elementV1, elementV2)) =>
        dpSum + elementV1 * elementV2
    })
  }

  /**
   * Compute the sum of the absolute value of each element of the vector.
   */
  def absoluteValue(v: Vector): Double = {
    (0 until v.cardinality).foldLeft(0.0)({
      case (absval, dimension) => absval + Math.abs(v.valueAt(dimension))
    })
  }

  def absElemDiff(v1: Vector, v2: Vector): Double =
    v1.zip(v2).foldLeft(0.0)({
      case (sum, (_, value1, value2)) =>
        sum + Math.abs(value1 - value2)
    })

  def rankFnOrdering[T](f: Vector => Double): ((T, Vector)) => Double =
    (x: (T, Vector)) => f(x._2)

}