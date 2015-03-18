/*
 * Cosine similarity function.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.wordcount

import mlbigbook.data.Vector

/** Contains implementation for cosine similarity function. */
object Similarity {

  import mlbigbook.data.Vector._

  /**
   * Computes the cosine similairty between two vectors, which is defined as:
   *
   *                  | v1 * v2 |
   *               -----------------
   *                  |v1| * |v2|
   *
   * where * is dot product and |...| is L1 norm.
   */
  def cosine(v1: Vector, v2: Vector): Double = {
    Math.abs(dotProduct(v1, v2)) / (absoluteValue(v1) * absoluteValue(v2))
  }

}