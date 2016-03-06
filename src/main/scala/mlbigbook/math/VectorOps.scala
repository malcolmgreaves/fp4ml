package mlbigbook.math

import breeze.linalg.operators._

import scala.language.{ higherKinds, implicitConversions }
import scala.reflect.ClassTag

/**
 * An abstraction specifying operations one may perform using vectors and
 * scalar values. These operations include element-wise & scalar
 * multiplication, division, addition, and subtraction. Support for the dot
 * product of two vectors is also included. As well as methods to construct new
 * vector instances.
 */
trait VectorOps[V[_]] {

  /**
   * Creates a new vector of the given size where each element has the
   * input value, named value.
   */
  def fill[A](size: Int)(value: => A): V[A]

  def valueAt[A](v: V[A])(index: Int): A

  def toSeq[A](v: V[A]): Seq[A]

  def map[A, B: ClassTag](v: V[A])(f: A => B): V[B]

  def size(v: V[_]): Int

}

