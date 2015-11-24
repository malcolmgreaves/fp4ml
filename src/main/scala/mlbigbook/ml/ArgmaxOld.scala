package mlbigbook.ml

import fif.Data
import Data.ops._

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

/**
 * Generic algorithm for finding the maximal argument. Uses the `Val`
 * typeclass as evidence of an argument's value.
 */
object ArgmaxOld {

  val emptyError =
    new IllegalArgumentException("Cannot call Argmax.apply on empty elements.")

  /**
   * Finds the maximal argument of `elements` in linear time. Uses the `Val`
   * typeclass as evidence of an argument's value.
   *
   * throws IllegalArgumentException Iff `elements` is empty.
   */
  def apply[B: ClassTag: Val](elements: mlbigbook.data.DataClass[B]): B =
    if (elements isEmpty)
      throw emptyError

    else
      elements
        .reduce[B] {
          case (a, b) =>
            if (Val[B].valueOf(a) > Val[B].valueOf(b))
              a
            else
              b
        }
}

object Argmax {

  /**
   * Finds the maximal argument of `elements` in linear time. Uses the `Val`
   * typeclass as evidence of an argument's value.
   */
  def apply[D[_]: Data, B: ClassTag: Val](elements: D[B]): Option[B] =
    if (elements isEmpty)
      None

    else
      Some(
        elements.reduce {
          case (a, b) =>
            if (Val[B].valueOf(a) > Val[B].valueOf(b))
              a
            else
              b
        }
      )

}

object Argmin {

  def apply[D[_]: Data, B: ClassTag: Val](elements: D[B]): Option[B] =
    Argmax(elements)(implicitly[Data[D]], implicitly[ClassTag[B]], Val.inverse)
}

/**
 * Typeclass for giving a value to a type `X`.
 */
trait Val[-X] {
  def valueOf(a: X): Double
}

object Val {

  def apply[V: Val]: Val[V] = implicitly[Val[V]]

  def inverse[V: Val]: Val[V] = {
    val original = Val[V]
    new Val[V] {
      def valueOf(a: V) = -original.valueOf(a)
    }
  }
}

object TupleVal1 {
  def apply[X] = new TupleVal1[X] {}
}

/**
 * Value for a tuple of (Double, X) type.
 */
trait TupleVal1[X] extends Val[(Double, X)] {
  override def valueOf(a: (Double, X)): Double =
    a._1
}

object TupleVal2 {
  def apply[X] = new TupleVal2[X] {}
}

/**
 * Value for a tuple of (X, Double) type.
 */
trait TupleVal2[X] extends Val[(X, Double)] {
  override def valueOf(a: (X, Double)): Double =
    a._2
}