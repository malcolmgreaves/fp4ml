package mlbigbook.math

import breeze.math.Semiring
import breeze.linalg.{ Vector, SparseVector, DenseVector }
import breeze.linalg.operators._
import breeze.storage.Zero
import com.github.fommil.netlib.BLAS
import com.github.fommil.netlib.BLAS._
import spire.syntax.cfor._

import scala.language.{ higherKinds, implicitConversions }
import scala.reflect.ClassTag

/**
 * An abstraction specifying operations one may perform using vectors and
 * scalar values. These operations include element-wise & scalar
 * multiplication, division, addition, and subtraction. Support for the dot
 * product of two vectors is also included. As well as methods to construct new
 * vector instances.
 */
abstract class MathVectorOps[N: Numeric: Zero: Semiring, V[_]]
    extends VectorOps[V] {

  /**
   * Creates a new vector of the input size where each element has value 0.
   */
  def zeros(size: Int): V[N]

  /**
   * Creates a new vector of the input size where each element has value 1.
   */
  def ones(size: Int): V[N]

  /**
   * Change every element of a vector V using the function f.
   * No side effects.
   */
  def map[B: ClassTag: Numeric: Zero](v: V[N])(f: N => B): V[B]

  /**
   * Perform
   */
  //  def aggregate[B : ClassTag : Numeric : Zero](v : V[N])(zero: B)(combine: (B, N) => B, reduce: (B, B) => B): B

  //  def reduce[A : ClassTag, A1 >: A : ClassTag](v: V[A])(r: (A1, A1) => A1): A1

  /**
   * Create a new vector of the input size where each element has the value v.
   */
  def fill[A: ClassTag: Zero](size: Int)(v: => A): V[A]

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

  @inline private[math] def vecCopyToSeq[@specialized A: ClassTag](src: Array[A]): Seq[A] =
    if (src == null || src.isEmpty)
      Seq.empty[A]
    else {
      val s = new Array[A](src.length)
      System.arraycopy(src, 0, s, 0, src.length)
      s.toSeq
    }

  import Zero._
  //  import algebra.std.all._

  lazy implicit val semiDouble: Semiring[Double] = Semiring.semiringD
  lazy implicit val semiFloat: Semiring[Float] = Semiring.semiringFloat
  lazy implicit val semiLong: Semiring[Long] = Semiring.semiringLong
  lazy implicit val semiInt: Semiring[Int] = Semiring.semiringInt
}