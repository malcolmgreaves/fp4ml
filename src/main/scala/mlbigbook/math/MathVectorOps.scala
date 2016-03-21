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

  @inline private[this] def vecCopyToSeq[@specialized A: ClassTag](src: Array[A]): Seq[A] =
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
  //
  //
  // Implementations for MathVectorOps for DenseVector instances.
  //
  //

  /**
   * Base partial implementation for DenseVectors. Implements the VectorOps
   * methods for the DenseVector type. Also defines the zeros, ones methds
   * of MathVectorOps.
   */
  protected abstract class Dense[@specialized N: Numeric: Zero: Semiring: ClassTag]
      extends MathVectorOps[N, DenseVector]
      with VectorOps[DenseVector] {

    override def zeros(size: Int): DenseVector[N] =
      DenseVector.zeros[N](size)

    override def ones(size: Int): DenseVector[N] =
      DenseVector.ones[N](size)(
        implicitly[ClassTag[N]],
        implicitly[Semiring[N]]
      )

    override def fill[A: ClassTag: Zero](size: Int)(value: => A) =
      DenseVector.fill(size)(value)

    override def toSeq[A: ClassTag](v: DenseVector[A]): Seq[A] =
      vecCopyToSeq(v.toArray)

    override def size(v: DenseVector[_]): Int =
      v.length

    override def valueAt[A](v: DenseVector[A])(index: Int): A =
      v(index)

    override def map[B: ClassTag: Numeric: Zero](v: DenseVector[N])(f: N => B): DenseVector[B] =
      v.map(f)

    //    override def aggregate[B : ClassTag : Numeric : Zero](v: DenseVector[N])(zero: B)(combine: (B, N) => B, reduce: (B,B) => B): B =
    //      map(v) { n => combine(zero, n) }
    //        .reduceLeft[B] {
    //          case (b1, b2) => reduce(b1, b2)
    //        }
  }

  /**
   * Implementation for DenseVector[Double].
   */
  object DoubleDenseMathVector extends Dense[Double] {
    override val addV = DenseVector.dv_dv_Op_Double_OpAdd
    override val addS = DenseVector.dv_s_Op_Double_OpAdd
    override val subV = DenseVector.dv_dv_Op_Double_OpSub
    override val subS = DenseVector.dv_s_Op_Double_OpSub
    override val dot = new OpMulInner.Impl2[DenseVector[Double], DenseVector[Double], Double] {

      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(b.length == a.length, "Vectors must be the same length!")
        val boff =
          if (b.stride >= 0) b.offset
          else b.offset + b.stride * (b.length - 1)
        val aoff =
          if (a.stride >= 0) a.offset
          else a.offset + a.stride * (a.length - 1)
        //        BLAS.getInstance().sdot(
        //          a.length, b.data, boff, b.stride, a.data, aoff, a.stride
        //        )

        //        var agg = 0.0
        //        cfor(0)(_ < a.length, _ += 1) { i =>
        //          agg += a(i) * b(i)
        //        }
        //        agg

        // TODO: Need to take offset into account !!!

        ???
      }

      implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpMulInner.type, Double]].register(this)
    }

    override val divS = DenseVector.dv_s_Op_Double_OpDiv
    override val mulS = DenseVector.dv_s_Op_Double_OpMulScalar
    override val divV = DenseVector.dv_dv_Op_Double_OpDiv
    override val mulV = DenseVector.dv_dv_Op_Double_OpMulScalar
  }

  /**
   * Implementation for DenseVector[Float].
   */
  object FloatDenseMathVector extends Dense[Float] {
    override val addV = DenseVector.dv_dv_Op_Float_OpAdd
    override val addS = DenseVector.dv_s_Op_Float_OpAdd
    override val subV = DenseVector.dv_dv_Op_Float_OpSub
    override val subS = DenseVector.dv_s_Op_Float_OpSub
    override val dot = DenseVector.canDot_DV_DV_Float
    override val divS = DenseVector.dv_s_Op_Float_OpDiv
    override val mulS = DenseVector.dv_s_Op_Float_OpMulScalar
    override val divV = DenseVector.dv_dv_Op_Float_OpDiv
    override val mulV = DenseVector.dv_dv_Op_Float_OpMulScalar
  }

  /**
   * Implementation for DenseVector[Long].
   */
  object LongDenseMathVector extends Dense[Long] {
    override val addV = DenseVector.dv_dv_Op_Long_OpAdd
    override val addS = DenseVector.dv_s_Op_Long_OpAdd
    override val subV = DenseVector.dv_dv_Op_Long_OpSub
    override val subS = DenseVector.dv_s_Op_Long_OpSub
    override val dot = DenseVector.canDot_DV_DV_Long
    override val divS = DenseVector.dv_s_Op_Long_OpDiv
    override val mulS = DenseVector.dv_s_Op_Long_OpMulScalar
    override val divV = DenseVector.dv_dv_Op_Long_OpDiv
    override val mulV = DenseVector.dv_dv_Op_Long_OpMulScalar
  }

  /**
   * Implementation for DenseVector[Int].
   */
  object IntDenseMathVector extends Dense[Int] {
    override val addV = DenseVector.dv_dv_Op_Int_OpAdd
    override val addS = DenseVector.dv_s_Op_Int_OpAdd
    override val subV = DenseVector.dv_dv_Op_Int_OpSub
    override val subS = DenseVector.dv_s_Op_Int_OpSub
    override val dot = DenseVector.canDot_DV_DV_Int
    override val divS = DenseVector.dv_s_Op_Int_OpDiv
    override val mulS = DenseVector.dv_s_Op_Int_OpMulScalar
    override val divV = DenseVector.dv_dv_Op_Int_OpDiv
    override val mulV = DenseVector.dv_dv_Op_Int_OpMulScalar
  }

  //
  //
  // Implementations for MathVectorOps for SparseVector instances.
  //
  //

  /**
   * Base partial implementation for DenseVectors. Implements the VectorOps
   * methods for the DenseVector type. Also defines the zeros, ones methds
   * of MathVectorOps.
   */
  protected abstract class Sparse[@specialized N: Numeric: Zero: Semiring: ClassTag]
      extends MathVectorOps[N, SparseVector]
      with VectorOps[SparseVector] {

    override def zeros(size: Int) =
      SparseVector.zeros[N](size)

    override def ones(size: Int) =
      SparseVector.fill(size)(implicitly[Numeric[N]].one)

    override def fill[A: ClassTag: Zero](size: Int)(value: => A) =
      SparseVector.fill(size)(value)

    override def toSeq[A: ClassTag](v: SparseVector[A]) =
      vecCopyToSeq(v.toDenseVector.toArray)

    override def size(v: SparseVector[_]): Int =
      v.length

    override def valueAt[A](v: SparseVector[A])(index: Int) =
      v(index)

    import SparseVector._

    override def map[B: ClassTag: Numeric: Zero](v: SparseVector[N])(f: N => B): SparseVector[B] =
      v.map(f)
  }

  /**
   * Implementation for SparseVector[Double].
   */
  object DoubleSparseMathVector extends Sparse[Double] {
    override val subS = new OpSub.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
      override def apply(v: SparseVector[Double], v2: Double) = v.map { _ - v2 }
    }
    override val subV = SparseVector.implOps_SVT_SVT_eq_SVT_Double_OpSub

    override val addS = new OpAdd.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
      override def apply(v: SparseVector[Double], v2: Double) = v.map { _ + v2 }
    }
    override val addV = SparseVector.implOps_SVT_SVT_eq_SVT_Double_OpAdd

    override val dot = new OpMulInner.Impl2[SparseVector[Double], SparseVector[Double], Double] {
      override def apply(v: SparseVector[Double], v2: SparseVector[Double]) = v.dot(v2)
    }

    override val divS = new OpDiv.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
      override def apply(v: SparseVector[Double], v2: Double) = v.map { _ / v2 }
    }
    override val divV = SparseVector.implOps_SVT_SVT_eq_SVT_Double_OpDiv

    override val mulS = new OpMulScalar.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
      override def apply(v: SparseVector[Double], v2: Double) = v.map { _ * v2 }
    }
    override val mulV = SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Double
  }

  /**
   * Implementation for SparseVector[Float].
   */
  object FloatSparseMathVector extends Sparse[Float] {
    override val subS = new OpSub.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
      override def apply(v: SparseVector[Float], v2: Float) = v.map { _ - v2 }
    }
    override val subV = SparseVector.implOps_SVT_SVT_eq_SVT_Float_OpSub

    override val addS = new OpAdd.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
      override def apply(v: SparseVector[Float], v2: Float) = v.map { _ + v2 }
    }
    override val addV = SparseVector.implOps_SVT_SVT_eq_SVT_Float_OpAdd

    override val dot = new OpMulInner.Impl2[SparseVector[Float], SparseVector[Float], Float] {
      override def apply(v: SparseVector[Float], v2: SparseVector[Float]) = v.dot(v2)
    }

    override val divS = new OpDiv.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
      override def apply(v: SparseVector[Float], v2: Float) = v.map { _ / v2 }
    }
    override val divV = SparseVector.implOps_SVT_SVT_eq_SVT_Float_OpDiv

    override val mulS = new OpMulScalar.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
      override def apply(v: SparseVector[Float], v2: Float) = v.map { _ * v2 }
    }
    override val mulV = SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Float
  }

  /**
   * Implementation for SparseVector[Long].
   */
  object LongSparseMathVector extends Sparse[Long] {
    override val subS = new OpSub.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
      override def apply(v: SparseVector[Long], v2: Long) = v.map { _ - v2 }
    }
    override val subV = SparseVector.implOps_SVT_SVT_eq_SVT_Long_OpSub

    override val addS = new OpAdd.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
      override def apply(v: SparseVector[Long], v2: Long) = v.map { _ + v2 }
    }
    override val addV = SparseVector.implOps_SVT_SVT_eq_SVT_Long_OpAdd

    override val dot = new OpMulInner.Impl2[SparseVector[Long], SparseVector[Long], Long] {
      override def apply(v: SparseVector[Long], v2: SparseVector[Long]) = v.dot(v2)
    }

    override val divS = new OpDiv.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
      override def apply(v: SparseVector[Long], v2: Long) = v.map { _ / v2 }
    }
    override val divV = SparseVector.implOps_SVT_SVT_eq_SVT_Long_OpDiv

    override val mulS = new OpMulScalar.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
      override def apply(v: SparseVector[Long], v2: Long) = v.map { _ * v2 }
    }
    override val mulV = SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Long
  }

  /**
   * Implementation for SparseVector[Int].
   */
  object IntSparseMathVector extends Sparse[Int] {
    override val subS = new OpSub.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
      override def apply(v: SparseVector[Int], v2: Int) = v.map { _ - v2 }
    }
    override val subV = SparseVector.implOps_SVT_SVT_eq_SVT_Int_OpSub

    override val addS = new OpAdd.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
      override def apply(v: SparseVector[Int], v2: Int) = v.map { _ + v2 }
    }
    override val addV = SparseVector.implOps_SVT_SVT_eq_SVT_Int_OpAdd

    override val dot = new OpMulInner.Impl2[SparseVector[Int], SparseVector[Int], Int] {
      override def apply(v: SparseVector[Int], v2: SparseVector[Int]) = v.dot(v2)
    }

    override val divS = new OpDiv.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
      override def apply(v: SparseVector[Int], v2: Int) = v.map { _ / v2 }
    }
    override val divV = SparseVector.implOps_SVT_SVT_eq_SVT_Int_OpDiv

    override val mulS = new OpMulScalar.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
      override def apply(v: SparseVector[Int], v2: Int) = v.map { _ * v2 }
    }
    override val mulV = SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Int
  }

}