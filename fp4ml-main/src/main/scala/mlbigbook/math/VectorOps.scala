package mlbigbook.math

import simulacrum.typeclass

import scala.language.higherKinds
import scala.reflect.ClassTag

/**
 * An abstraction specifying operations one may perform using vectors and
 * scalar values. These operations include element-wise & scalar
 * multiplication, division, addition, and subtraction. Support for the dot
 * product of two vectors is also included. As well as methods to construct new
 * vector instances.
 */
@typeclass trait VectorOps[V[_]] {

  def apply[A](v: V[A])(index: Int): A

  def toSeq[A: ClassTag](v: V[A]): Seq[A]

  def size(v: V[_]): Int

  def foreach[A](v : V[A])(f: A => Any): Unit

}