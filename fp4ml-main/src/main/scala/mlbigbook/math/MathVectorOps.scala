package mlbigbook.math

import breeze.math.Semiring
import breeze.linalg.operators._
import breeze.storage.Zero

import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

/**
  * An abstraction specifying operations one may perform using vectors and
  * scalar values. These operations include element-wise & scalar
  * multiplication, division, addition, and subtraction. Support for the dot
  * product of two vectors is also included. As well as methods to construct new
  * vector instances.
  */
trait MathVectorOps[V[_]] extends VectorOps[V] {

  type N
  implicit val n: Fractional[N]
  implicit val z: Zero[N]
  implicit val s: Semiring[N]

  /**
    * Creates a new vector of the input size where each element has value 0.
    */
  def zeros(size: Int): V[N]

  /**
    * Creates a new vector of the input size where each element has value 1.
    */
  def ones(size: Int): V[N]

  protected lazy val zero = implicitly[Fractional[N]].zero
  protected lazy val one = implicitly[Fractional[N]].one

  /**
    * Change every element of a vector V using the function f.
    * No side effects.
    */
  def map[B: ClassTag: Fractional: Zero](v: V[N])(f: N => B): V[B]

  /**
    * Apply a binary combination operator, r, to pairs of elements from the
    * input vector, v. Note that the output of r shall be applied to both
    * vector elements as well as other, previous outputs from r. The order of
    * execution is not guaranteed. Therefore, it is important that r is
    * associative and communiative.
    */
  def reduce[A1 >: N: ClassTag](v: V[N])(r: (A1, A1) => A1): A1

  /**
    * From the starting value, zero, applies the function combine to elements
    * of the input vector v. This method evaluates to the final accumulated
    * value of this operation across all elements of the vector. Execution
    * order is not guaranteed, so combine must be side-effect free,
    * associative, and communicative.
    */
  def fold[B: ClassTag](v: V[N])(zero: B)(combine: (B, N) => B): B

  /**
    * Create a new vector of the input size where each element has the value v.
    */
  def fill[A: ClassTag: Zero](size: Int)(v: => A): V[A]

  /**
    * Performs a shallow copy of the vector's contents. Each element is copied
    * to a newly allocated vector of type V[N]. If N is a primitive or other
    * value type, then this will be a deep copy. Otherwise, the reference will
    * be copied.
    */
  def copy(v: V[N]): V[N]

  /**
    * Performs element-wise addition of two vectors.
    */
  val addV: OpAdd.Impl2[V[N], V[N], V[N]]

  /**
    * Adds a scalar to each element of a vector.
    */
  val addS: OpAdd.Impl2[V[N], N, V[N]]

  /**
    * Performs element-wise subtraction of two vectors.
    */
  val subV: OpSub.Impl2[V[N], V[N], V[N]]

  /**
    * Subtracts a scalar from each element of a vector.
    */
  val subS: OpSub.Impl2[V[N], N, V[N]]

  /**
    * Performs a dot product operation between two vectors,
    * which results in a scalar.
    */
  val dot: OpMulInner.Impl2[V[N], V[N], N]

  /**
    * Performs element-wise multiplication between two vectors.
    */
  val mulV: OpMulScalar.Impl2[V[N], V[N], V[N]]

  /**
    * Multiplies each vector element by a scalar.
    */
  val mulS: OpMulScalar.Impl2[V[N], N, V[N]]

  /**
    * Performs element-wise division between two vectors.
    */
  val divV: OpDiv.Impl2[V[N], V[N], V[N]]

  /**
    * Divides each vector element by a scalar.
    */
  val divS: OpDiv.Impl2[V[N], N, V[N]]

}

object MathVectorOps {

  type Type[Num, Vec[_]] = MathVectorOps[Vec] {
    type N = Num
  }

  object Implicits {
    // dense operations
    implicit val DoubleDenseVot = DoubleDenseMathVector
    implicit val FloatDenseVot = FloatDenseMathVector
    implicit val LongDenseVot = LongDenseMathVector
    implicit val IntDenseVot = IntDenseMathVector
    // sparse operations
    implicit val DoubleSparseVot = DoubleSparseMathVector
    implicit val FloatSparseVot = FloatSparseMathVector
    implicit val LongSparseVot = LongSparseMathVector
    implicit val IntSparseVot = IntSparseMathVector
  }

}
