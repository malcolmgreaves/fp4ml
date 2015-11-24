package mlbigbook.math

import breeze.linalg.{ SparseVector, NumericOps, DenseVector, Vector }
import breeze.linalg.operators._
import breeze.math.Semiring
import mlbigbook.math.VectorOpsT.Dense.DoubleDenseVectorOptsT

import scala.language.{ higherKinds, implicitConversions }

/**
 * An abstraction specifying operations one may perform using vectors and
 * scalar values. These operations include element-wise & scalar
 * multiplication, division, addition, and subtraction. Support for the dot
 * product of two vectors is also included. As well as methods to construct new
 * vector instances.
 */
abstract class VectorOpsT[@specialized N: Numeric, V[_] <: Vector[_]] {

  /**
   * Creates a new vector of the input size where each element has value 0.
   */
  def zeros(size: Int): V[N] =
    fill(size)(implicitly[Numeric[N]].zero)

  /**
   * Creates a new vector of the input size where each element has value 1.
   */
  def ones(size: Int): V[N] =
    fill(size)(implicitly[Numeric[N]].one)

  /**
   * Creates a new vector of the given size where each element has the
   * input value, named value.
   */
  def fill(size: Int)(value: N): V[N]

  def valueAt(v: V[N])(index: Int): N

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

object VectorOpsT {

  object Implicits {
    // dense operations
    implicit val DoubleDenseVot = Dense.DoubleDenseVectorOptsT
    implicit val FloatDenseVot = Dense.FloatDenseVectorOptsT
    implicit val LongDenseVot = Dense.LongDenseVectorOptsT
    implicit val IntDenseVot = Dense.IntDenseVectorOptsT
    // sparse operations
    implicit val DoubleSparseVot = Sparse.DoubleSparseVectorOptsT
    implicit val FloatSparseVot = Sparse.FloatSparseVectorOptsT
    implicit val LongSparseVot = Sparse.LongSparseVectorOptsT
    implicit val IntSparseVot = Sparse.IntSparseVectorOptsT
  }

  /**
   * Implementations for VectorOpsT for DenseVector instances.
   */
  object Dense {

    object DoubleDenseVectorOptsT extends VectorOpsT[Double, DenseVector] {

      override def zeros(size: Int): DenseVector[Double] =
        DenseVector.zeros(size)

      override def ones(size: Int): DenseVector[Double] =
        DenseVector.ones(size)

      override def fill(size: Int)(value: Double): DenseVector[Double] =
        DenseVector.fill(size)(value)

      override val addV: OpAdd.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] =
        DenseVector.dv_dv_Op_Double_OpAdd

      override val addS: OpAdd.Impl2[DenseVector[Double], Double, DenseVector[Double]] =
        DenseVector.dv_s_Op_Double_OpAdd

      val subV: OpSub.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] =
        DenseVector.dv_dv_Op_Double_OpSub

      val subS: OpSub.Impl2[DenseVector[Double], Double, DenseVector[Double]] =
        DenseVector.dv_s_Op_Double_OpSub

      override val dot: OpMulInner.Impl2[DenseVector[Double], DenseVector[Double], Double] =
        DenseVector.canDotD

      override val divS: OpDiv.Impl2[DenseVector[Double], Double, DenseVector[Double]] =
        DenseVector.dv_s_Op_Double_OpDiv

      override val mulS: OpMulScalar.Impl2[DenseVector[Double], Double, DenseVector[Double]] =
        DenseVector.dv_s_Op_Double_OpMulScalar

      override val divV: OpDiv.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] =
        DenseVector.dv_dv_Op_Double_OpDiv

      override val mulV: OpMulScalar.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] =
        DenseVector.dv_dv_Op_Double_OpMulScalar

      override def valueAt(v: DenseVector[Double])(index: Int): Double =
        v(index)
    }

    object FloatDenseVectorOptsT extends VectorOpsT[Float, DenseVector] {

      override def zeros(size: Int): DenseVector[Float] =
        DenseVector.zeros(size)

      override def ones(size: Int): DenseVector[Float] =
        DenseVector.ones(size)

      override def fill(size: Int)(value: Float): DenseVector[Float] =
        DenseVector.fill(size)(value)

      override val addV: OpAdd.Impl2[DenseVector[Float], DenseVector[Float], DenseVector[Float]] =
        DenseVector.dv_dv_Op_Float_OpAdd

      override val addS: OpAdd.Impl2[DenseVector[Float], Float, DenseVector[Float]] =
        DenseVector.dv_s_Op_Float_OpAdd

      val subV: OpSub.Impl2[DenseVector[Float], DenseVector[Float], DenseVector[Float]] =
        DenseVector.dv_dv_Op_Float_OpSub

      val subS: OpSub.Impl2[DenseVector[Float], Float, DenseVector[Float]] =
        DenseVector.dv_s_Op_Float_OpSub

      override val dot: OpMulInner.Impl2[DenseVector[Float], DenseVector[Float], Float] =
        DenseVector.canDot_DV_DV_Float

      override val divS: OpDiv.Impl2[DenseVector[Float], Float, DenseVector[Float]] =
        DenseVector.dv_s_Op_Float_OpDiv

      override val mulS: OpMulScalar.Impl2[DenseVector[Float], Float, DenseVector[Float]] =
        DenseVector.dv_s_Op_Float_OpMulScalar

      override val divV: OpDiv.Impl2[DenseVector[Float], DenseVector[Float], DenseVector[Float]] =
        DenseVector.dv_dv_Op_Float_OpDiv

      override val mulV: OpMulScalar.Impl2[DenseVector[Float], DenseVector[Float], DenseVector[Float]] =
        DenseVector.dv_dv_Op_Float_OpMulScalar

      override def valueAt(v: DenseVector[Float])(index: Int): Float =
        v(index)
    }

    object LongDenseVectorOptsT extends VectorOpsT[Long, DenseVector] {

      override def zeros(size: Int): DenseVector[Long] =
        DenseVector.zeros(size)

      override def ones(size: Int): DenseVector[Long] =
        DenseVector.ones(size)

      override def fill(size: Int)(value: Long): DenseVector[Long] =
        DenseVector.fill(size)(value)

      override val addV: OpAdd.Impl2[DenseVector[Long], DenseVector[Long], DenseVector[Long]] =
        DenseVector.dv_dv_Op_Long_OpAdd

      override val addS: OpAdd.Impl2[DenseVector[Long], Long, DenseVector[Long]] =
        DenseVector.dv_s_Op_Long_OpAdd

      val subV: OpSub.Impl2[DenseVector[Long], DenseVector[Long], DenseVector[Long]] =
        DenseVector.dv_dv_Op_Long_OpSub

      val subS: OpSub.Impl2[DenseVector[Long], Long, DenseVector[Long]] =
        DenseVector.dv_s_Op_Long_OpSub

      override val dot: OpMulInner.Impl2[DenseVector[Long], DenseVector[Long], Long] =
        DenseVector.canDot_DV_DV_Long

      override val divS: OpDiv.Impl2[DenseVector[Long], Long, DenseVector[Long]] =
        DenseVector.dv_s_Op_Long_OpDiv

      override val mulS: OpMulScalar.Impl2[DenseVector[Long], Long, DenseVector[Long]] =
        DenseVector.dv_s_Op_Long_OpMulScalar

      override val divV: OpDiv.Impl2[DenseVector[Long], DenseVector[Long], DenseVector[Long]] =
        DenseVector.dv_dv_Op_Long_OpDiv

      override val mulV: OpMulScalar.Impl2[DenseVector[Long], DenseVector[Long], DenseVector[Long]] =
        DenseVector.dv_dv_Op_Long_OpMulScalar

      override def valueAt(v: DenseVector[Long])(index: Int): Long =
        v(index)
    }

    object IntDenseVectorOptsT extends VectorOpsT[Int, DenseVector] {

      override def zeros(size: Int): DenseVector[Int] =
        DenseVector.zeros(size)

      override def ones(size: Int): DenseVector[Int] =
        DenseVector.ones(size)

      override def fill(size: Int)(value: Int): DenseVector[Int] =
        DenseVector.fill(size)(value)

      override val addV: OpAdd.Impl2[DenseVector[Int], DenseVector[Int], DenseVector[Int]] =
        DenseVector.dv_dv_Op_Int_OpAdd

      override val addS: OpAdd.Impl2[DenseVector[Int], Int, DenseVector[Int]] =
        DenseVector.dv_s_Op_Int_OpAdd

      val subV: OpSub.Impl2[DenseVector[Int], DenseVector[Int], DenseVector[Int]] =
        DenseVector.dv_dv_Op_Int_OpSub

      val subS: OpSub.Impl2[DenseVector[Int], Int, DenseVector[Int]] =
        DenseVector.dv_s_Op_Int_OpSub

      override val dot: OpMulInner.Impl2[DenseVector[Int], DenseVector[Int], Int] =
        DenseVector.canDot_DV_DV_Int

      override val divS: OpDiv.Impl2[DenseVector[Int], Int, DenseVector[Int]] =
        DenseVector.dv_s_Op_Int_OpDiv

      override val mulS: OpMulScalar.Impl2[DenseVector[Int], Int, DenseVector[Int]] =
        DenseVector.dv_s_Op_Int_OpMulScalar

      override val divV: OpDiv.Impl2[DenseVector[Int], DenseVector[Int], DenseVector[Int]] =
        DenseVector.dv_dv_Op_Int_OpDiv

      override val mulV: OpMulScalar.Impl2[DenseVector[Int], DenseVector[Int], DenseVector[Int]] =
        DenseVector.dv_dv_Op_Int_OpMulScalar

      override def valueAt(v: DenseVector[Int])(index: Int): Int =
        v(index)
    }
  }

  /**
   * Implementations for VectorOpsT for SparseVector instances.
   */
  object Sparse {

    object DoubleSparseVectorOptsT extends VectorOpsT[Double, SparseVector] {

      override def zeros(size: Int): SparseVector[Double] =
        SparseVector.zeros(size)

      override def fill(size: Int)(value: Double): SparseVector[Double] =
        SparseVector.fill(size)(value)

      override val addV: OpAdd.Impl2[SparseVector[Double], SparseVector[Double], SparseVector[Double]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Double_OpAdd

      override val addS: OpAdd.Impl2[SparseVector[Double], Double, SparseVector[Double]] =
        new OpAdd.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
          override def apply(v: SparseVector[Double], v2: Double): SparseVector[Double] =
            v.map { _ + v2 }
        }

      val subV: OpSub.Impl2[SparseVector[Double], SparseVector[Double], SparseVector[Double]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Double_OpSub

      val subS: OpSub.Impl2[SparseVector[Double], Double, SparseVector[Double]] =
        new OpSub.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
          override def apply(v: SparseVector[Double], v2: Double): SparseVector[Double] =
            v.map { _ - v2 }
        }

      override val dot: OpMulInner.Impl2[SparseVector[Double], SparseVector[Double], Double] =
        new OpMulInner.Impl2[SparseVector[Double], SparseVector[Double], Double] {
          override def apply(v: SparseVector[Double], v2: SparseVector[Double]): Double =
            v.dot(v2)
        }

      override val divS: OpDiv.Impl2[SparseVector[Double], Double, SparseVector[Double]] =
        new OpDiv.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
          override def apply(v: SparseVector[Double], v2: Double): SparseVector[Double] =
            v.map { _ / v2 }
        }

      override val mulS: OpMulScalar.Impl2[SparseVector[Double], Double, SparseVector[Double]] =
        new OpMulScalar.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
          override def apply(v: SparseVector[Double], v2: Double): SparseVector[Double] =
            v.map { _ * v2 }
        }

      override val divV: OpDiv.Impl2[SparseVector[Double], SparseVector[Double], SparseVector[Double]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Double_OpDiv

      override val mulV: OpMulScalar.Impl2[SparseVector[Double], SparseVector[Double], SparseVector[Double]] =
        SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Double

      override def valueAt(v: SparseVector[Double])(index: Int): Double =
        v(index)
    }

    object FloatSparseVectorOptsT extends VectorOpsT[Float, SparseVector] {

      override def zeros(size: Int): SparseVector[Float] =
        SparseVector.zeros(size)

      override def fill(size: Int)(value: Float): SparseVector[Float] =
        SparseVector.fill(size)(value)

      override val addV: OpAdd.Impl2[SparseVector[Float], SparseVector[Float], SparseVector[Float]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Float_OpAdd

      override val addS: OpAdd.Impl2[SparseVector[Float], Float, SparseVector[Float]] =
        new OpAdd.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
          override def apply(v: SparseVector[Float], v2: Float): SparseVector[Float] =
            v.map { _ + v2 }
        }

      val subV: OpSub.Impl2[SparseVector[Float], SparseVector[Float], SparseVector[Float]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Float_OpSub

      val subS: OpSub.Impl2[SparseVector[Float], Float, SparseVector[Float]] =
        new OpSub.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
          override def apply(v: SparseVector[Float], v2: Float): SparseVector[Float] =
            v.map { _ - v2 }
        }

      override val dot: OpMulInner.Impl2[SparseVector[Float], SparseVector[Float], Float] =
        new OpMulInner.Impl2[SparseVector[Float], SparseVector[Float], Float] {
          override def apply(v: SparseVector[Float], v2: SparseVector[Float]): Float =
            v.dot(v2)
        }

      override val divS: OpDiv.Impl2[SparseVector[Float], Float, SparseVector[Float]] =
        new OpDiv.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
          override def apply(v: SparseVector[Float], v2: Float): SparseVector[Float] =
            v.map { _ / v2 }
        }

      override val mulS: OpMulScalar.Impl2[SparseVector[Float], Float, SparseVector[Float]] =
        new OpMulScalar.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
          override def apply(v: SparseVector[Float], v2: Float): SparseVector[Float] =
            v.map { _ * v2 }
        }

      override val divV: OpDiv.Impl2[SparseVector[Float], SparseVector[Float], SparseVector[Float]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Float_OpDiv

      override val mulV: OpMulScalar.Impl2[SparseVector[Float], SparseVector[Float], SparseVector[Float]] =
        SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Float

      override def valueAt(v: SparseVector[Float])(index: Int): Float =
        v(index)
    }

    object LongSparseVectorOptsT extends VectorOpsT[Long, SparseVector] {

      override def zeros(size: Int): SparseVector[Long] =
        SparseVector.zeros(size)

      override def fill(size: Int)(value: Long): SparseVector[Long] =
        SparseVector.fill(size)(value)

      override val addV: OpAdd.Impl2[SparseVector[Long], SparseVector[Long], SparseVector[Long]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Long_OpAdd

      override val addS: OpAdd.Impl2[SparseVector[Long], Long, SparseVector[Long]] =
        new OpAdd.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
          override def apply(v: SparseVector[Long], v2: Long): SparseVector[Long] =
            v.map { _ + v2 }
        }

      val subV: OpSub.Impl2[SparseVector[Long], SparseVector[Long], SparseVector[Long]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Long_OpSub

      val subS: OpSub.Impl2[SparseVector[Long], Long, SparseVector[Long]] =
        new OpSub.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
          override def apply(v: SparseVector[Long], v2: Long): SparseVector[Long] =
            v.map { _ - v2 }
        }

      override val dot: OpMulInner.Impl2[SparseVector[Long], SparseVector[Long], Long] =
        new OpMulInner.Impl2[SparseVector[Long], SparseVector[Long], Long] {
          override def apply(v: SparseVector[Long], v2: SparseVector[Long]): Long =
            v.dot(v2)
        }

      override val divS: OpDiv.Impl2[SparseVector[Long], Long, SparseVector[Long]] =
        new OpDiv.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
          override def apply(v: SparseVector[Long], v2: Long): SparseVector[Long] =
            v.map { _ / v2 }
        }

      override val mulS: OpMulScalar.Impl2[SparseVector[Long], Long, SparseVector[Long]] =
        new OpMulScalar.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
          override def apply(v: SparseVector[Long], v2: Long): SparseVector[Long] =
            v.map { _ * v2 }
        }

      override val divV: OpDiv.Impl2[SparseVector[Long], SparseVector[Long], SparseVector[Long]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Long_OpDiv

      override val mulV: OpMulScalar.Impl2[SparseVector[Long], SparseVector[Long], SparseVector[Long]] =
        SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Long

      override def valueAt(v: SparseVector[Long])(index: Int): Long =
        v(index)
    }

    object IntSparseVectorOptsT extends VectorOpsT[Int, SparseVector] {

      override def zeros(size: Int): SparseVector[Int] =
        SparseVector.zeros(size)

      override def fill(size: Int)(value: Int): SparseVector[Int] =
        SparseVector.fill(size)(value)

      override val addV: OpAdd.Impl2[SparseVector[Int], SparseVector[Int], SparseVector[Int]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Int_OpAdd

      override val addS: OpAdd.Impl2[SparseVector[Int], Int, SparseVector[Int]] =
        new OpAdd.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
          override def apply(v: SparseVector[Int], v2: Int): SparseVector[Int] =
            v.map { _ + v2 }
        }

      val subV: OpSub.Impl2[SparseVector[Int], SparseVector[Int], SparseVector[Int]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Int_OpSub

      val subS: OpSub.Impl2[SparseVector[Int], Int, SparseVector[Int]] =
        new OpSub.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
          override def apply(v: SparseVector[Int], v2: Int): SparseVector[Int] =
            v.map { _ - v2 }
        }

      override val dot: OpMulInner.Impl2[SparseVector[Int], SparseVector[Int], Int] =
        new OpMulInner.Impl2[SparseVector[Int], SparseVector[Int], Int] {
          override def apply(v: SparseVector[Int], v2: SparseVector[Int]): Int =
            v.dot(v2)
        }

      override val divS: OpDiv.Impl2[SparseVector[Int], Int, SparseVector[Int]] =
        new OpDiv.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
          override def apply(v: SparseVector[Int], v2: Int): SparseVector[Int] =
            v.map { _ / v2 }
        }

      override val mulS: OpMulScalar.Impl2[SparseVector[Int], Int, SparseVector[Int]] =
        new OpMulScalar.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
          override def apply(v: SparseVector[Int], v2: Int): SparseVector[Int] =
            v.map { _ * v2 }
        }

      override val divV: OpDiv.Impl2[SparseVector[Int], SparseVector[Int], SparseVector[Int]] =
        SparseVector.implOps_SVT_SVT_eq_SVT_Int_OpDiv

      override val mulV: OpMulScalar.Impl2[SparseVector[Int], SparseVector[Int], SparseVector[Int]] =
        SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Int

      override def valueAt(v: SparseVector[Int])(index: Int): Int =
        v(index)
    }
  }

}