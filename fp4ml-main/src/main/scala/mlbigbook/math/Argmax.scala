package mlbigbook.math

import fif.Data

import scala.reflect.ClassTag

/**
 * Generic algorithm for finding the maximal argument. Uses the `Val`
 * type class as evidence of an argument's value.
 */
object Argmax {

  import Data.ops._

  /**
   * Finds the maximal argument of `elements` in linear time. Uses the `Val`
   * type class as evidence of an argument's value.
   *
   * throws IllegalArgumentException Iff `elements` is empty.
   */
  def apply[T: Val: ClassTag, D[_]: Data](elements: D[T]): Option[T] =
    if (elements isEmpty)
      None
    else
      Some(applyUnsafe(elements))

  def applyUnsafe[T: Val: ClassTag, D[_]: Data](elements: D[T]): T = {
    val v = Val[T]
    elements.reduce {
      case (a, b) =>
        if (v.n.lt(v.valueOf(a), v.valueOf(b)))
          a
        else
          b
    }
  }
}