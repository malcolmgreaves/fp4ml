package mlbigbook.math

import breeze.linalg.{ SparseVector, DenseVector }
import breeze.linalg.operators._
import breeze.storage.Zero

import scala.language.{ higherKinds, implicitConversions }
import scala.reflect.ClassTag

/**
 * An abstraction specifying operations one may perform using vectors and
 * scalar values. These operations include element-wise & scalar
 * multiplication, division, addition, and subtraction. Support for the dot
 * product of two vectors is also included. As well as methods to construct new
 * vector instances.
 */
trait MathVectorOps[@specialized N, V[_]] {

  implicit def num: Numeric[N]

  /**
   * Creates a new vector of the input size where each element has value 0.
   */
  def zeros(size: Int): V[N]

  /**
   * Creates a new vector of the input size where each element has value 1.
   */
  def ones(size: Int): V[N]

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

  private[this] def vecCopyToSeq[A](src: Array[A]): Seq[A] =
    if (src == null)
      Seq.empty[A]
    else {
      val s = new Array[A](src.length)
      System.arraycopy(src, 0, s, 0, src.length)
      s.toSeq
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
  private[this] trait Dense[N]
      extends MathVectorOps[N, DenseVector]
      with VectorOps[DenseVector] {

    override def zeros(size: Int): DenseVector[N] =
      DenseVector.zeros[N](size)

    override def ones(size: Int): DenseVector[N] =
      DenseVector.ones[N](size)

    override def toSeq[A](v: DenseVector[A]): Seq[A] =
      vecCopyToSeq(v.toArray)

    override def size(v: DenseVector[_]): Int =
      v.length

    override def fill[A](size: Int)(value: => A): DenseVector[A] =
      DenseVector.fill(size)(value)

    override def valueAt[A](v: DenseVector[A])(index: Int): A =
      v(index)

    override def map[A, B: ClassTag](v: DenseVector[A])(f: A => B): DenseVector[B] =
      v.map(f)
  }

  /**
   * Implementation for DenseVector[Double].
   */
  object DoubleDenseMathVector extends Dense[Double] {
    override implicit val num = implicitly[Numeric[Double]]
    override val addV = DenseVector.dv_dv_Op_Double_OpAdd
    override val addS = DenseVector.dv_s_Op_Double_OpAdd
    override val subV = DenseVector.dv_dv_Op_Double_OpSub
    override val subS = DenseVector.dv_s_Op_Double_OpSub
    override val dot = DenseVector.canDotD
    override val divS = DenseVector.dv_s_Op_Double_OpDiv
    override val mulS = DenseVector.dv_s_Op_Double_OpMulScalar
    override val divV = DenseVector.dv_dv_Op_Double_OpDiv
    override val mulV = DenseVector.dv_dv_Op_Double_OpMulScalar
  }

  /**
   * Implementation for DenseVector[Float].
   */
  object FloatDenseMathVector extends Dense[Float] {
    override implicit val num = implicitly[Numeric[Float]]
    override val addV = DenseVector.dv_dv_Op_Float_OpAdd
    override val addS = DenseVector.dv_s_Op_Float_OpAdd
    override val subV = DenseVector.dv_dv_Op_Float_OpSub
    override val subS = DenseVector.dv_s_Op_Float_OpSub
    override val dot = DenseVector.canDotD
    override val divS = DenseVector.dv_s_Op_Float_OpDiv
    override val mulS = DenseVector.dv_s_Op_Float_OpMulScalar
    override val divV = DenseVector.dv_dv_Op_Float_OpDiv
    override val mulV = DenseVector.dv_dv_Op_Float_OpMulScalar
  }

  /**
   * Implementation for DenseVector[Long].
   */
  object LongDenseMathVector extends Dense[Long] {
    override implicit val num = implicitly[Numeric[Long]]
    override val addV = DenseVector.dv_dv_Op_Long_OpAdd
    override val addS = DenseVector.dv_s_Op_Long_OpAdd
    override val subV = DenseVector.dv_dv_Op_Long_OpSub
    override val subS = DenseVector.dv_s_Op_Long_OpSub
    override val dot = DenseVector.canDotD
    override val divS = DenseVector.dv_s_Op_Long_OpDiv
    override val mulS = DenseVector.dv_s_Op_Long_OpMulScalar
    override val divV = DenseVector.dv_dv_Op_Long_OpDiv
    override val mulV = DenseVector.dv_dv_Op_Long_OpMulScalar
  }

  /**
   * Implementation for DenseVector[Int].
   */
  object IntDenseMathVector extends Dense[Int] {
    override implicit val num = implicitly[Numeric[Int]]
    override val addV = DenseVector.dv_dv_Op_Int_OpAdd
    override val addS = DenseVector.dv_s_Op_Int_OpAdd
    override val subV = DenseVector.dv_dv_Op_Int_OpSub
    override val subS = DenseVector.dv_s_Op_Int_OpSub
    override val dot = DenseVector.canDotD
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
  private[this] trait Sparse[N]
      extends MathVectorOps[N, SparseVector]
      with VectorOps[DenseVector] {

    override def zeros(size: Int) =
      SparseVector.zeros[N](size)

    override def ones(size: Int) =
      SparseVector.fill(size)(num.one)

    override def toSeq[A](v: SparseVector[A]) =
      vecCopyToSeq(v.toDenseVector.toArray)

    override def size(v: DenseVector[_]): Int =
      v.length

    override def fill[A](size: Int)(value: => A) =
      SparseVector.fill(size)(value)

    override def valueAt[A](v: SparseVector[A])(index: Int) =
      v(index)

    override def map[A, B: ClassTag](v: SparseVector[A])(f: A => B) =
      v.map(f)
  }
  //
  //    object DoubleSparseMathVector extends MathVectorOps[Double, SparseVector] {
  //
  //      override def zeros(size: Int): SparseVector[Double] =
  //        SparseVector.zeros(size)
  //
  //      override def fill(size: Int)(value: Double): SparseVector[Double] =
  //        SparseVector.fill(size)(value)
  //
  //      override val addV: OpAdd.Impl2[SparseVector[Double], SparseVector[Double], SparseVector[Double]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Double_OpAdd
  //
  //      override val addS: OpAdd.Impl2[SparseVector[Double], Double, SparseVector[Double]] =
  //        new OpAdd.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
  //          override def apply(v: SparseVector[Double], v2: Double): SparseVector[Double] =
  //            v.map { _ + v2 }
  //        }
  //
  //      val subV: OpSub.Impl2[SparseVector[Double], SparseVector[Double], SparseVector[Double]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Double_OpSub
  //
  //      val subS: OpSub.Impl2[SparseVector[Double], Double, SparseVector[Double]] =
  //        new OpSub.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
  //          override def apply(v: SparseVector[Double], v2: Double): SparseVector[Double] =
  //            v.map { _ - v2 }
  //        }
  //
  //      override val dot: OpMulInner.Impl2[SparseVector[Double], SparseVector[Double], Double] =
  //        new OpMulInner.Impl2[SparseVector[Double], SparseVector[Double], Double] {
  //          override def apply(v: SparseVector[Double], v2: SparseVector[Double]): Double =
  //            v.dot(v2)
  //        }
  //
  //      override val divS: OpDiv.Impl2[SparseVector[Double], Double, SparseVector[Double]] =
  //        new OpDiv.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
  //          override def apply(v: SparseVector[Double], v2: Double): SparseVector[Double] =
  //            v.map { _ / v2 }
  //        }
  //
  //      override val mulS: OpMulScalar.Impl2[SparseVector[Double], Double, SparseVector[Double]] =
  //        new OpMulScalar.Impl2[SparseVector[Double], Double, SparseVector[Double]] {
  //          override def apply(v: SparseVector[Double], v2: Double): SparseVector[Double] =
  //            v.map { _ * v2 }
  //        }
  //
  //      override val divV: OpDiv.Impl2[SparseVector[Double], SparseVector[Double], SparseVector[Double]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Double_OpDiv
  //
  //      override val mulV: OpMulScalar.Impl2[SparseVector[Double], SparseVector[Double], SparseVector[Double]] =
  //        SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Double
  //
  //      override def valueAt(v: SparseVector[Double])(index: Int): Double =
  //        v(index)
  //
  //      override def toSeq(v: SparseVector[Double]): Seq[Double] =
  //        v.toArray.toSeq
  //
  //      override def map[B: ClassTag](v: SparseVector[Double])(f: (Double) => B): SparseVector[B] = {
  //        implicit val _0: Zero[B] = new Zero[B] { override val zero: B = null.asInstanceOf[B] }
  //        implicit val _1 = SparseVector.canMapValues[SparseVector[Double], B]
  //        v.map(f)
  //      }
  //
  //      override def size(v: SparseVector[Double]): Int =
  //        v.length
  //    }
  //
  //    object FloatSparseMathVector extends MathVectorOps[Float, SparseVector] {
  //
  //      override def zeros(size: Int): SparseVector[Float] =
  //        SparseVector.zeros(size)
  //
  //      override def fill(size: Int)(value: Float): SparseVector[Float] =
  //        SparseVector.fill(size)(value)
  //
  //      override val addV: OpAdd.Impl2[SparseVector[Float], SparseVector[Float], SparseVector[Float]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Float_OpAdd
  //
  //      override val addS: OpAdd.Impl2[SparseVector[Float], Float, SparseVector[Float]] =
  //        new OpAdd.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
  //          override def apply(v: SparseVector[Float], v2: Float): SparseVector[Float] =
  //            v.map { _ + v2 }
  //        }
  //
  //      val subV: OpSub.Impl2[SparseVector[Float], SparseVector[Float], SparseVector[Float]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Float_OpSub
  //
  //      val subS: OpSub.Impl2[SparseVector[Float], Float, SparseVector[Float]] =
  //        new OpSub.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
  //          override def apply(v: SparseVector[Float], v2: Float): SparseVector[Float] =
  //            v.map { _ - v2 }
  //        }
  //
  //      override val dot: OpMulInner.Impl2[SparseVector[Float], SparseVector[Float], Float] =
  //        new OpMulInner.Impl2[SparseVector[Float], SparseVector[Float], Float] {
  //          override def apply(v: SparseVector[Float], v2: SparseVector[Float]): Float =
  //            v.dot(v2)
  //        }
  //
  //      override val divS: OpDiv.Impl2[SparseVector[Float], Float, SparseVector[Float]] =
  //        new OpDiv.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
  //          override def apply(v: SparseVector[Float], v2: Float): SparseVector[Float] =
  //            v.map { _ / v2 }
  //        }
  //
  //      override val mulS: OpMulScalar.Impl2[SparseVector[Float], Float, SparseVector[Float]] =
  //        new OpMulScalar.Impl2[SparseVector[Float], Float, SparseVector[Float]] {
  //          override def apply(v: SparseVector[Float], v2: Float): SparseVector[Float] =
  //            v.map { _ * v2 }
  //        }
  //
  //      override val divV: OpDiv.Impl2[SparseVector[Float], SparseVector[Float], SparseVector[Float]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Float_OpDiv
  //
  //      override val mulV: OpMulScalar.Impl2[SparseVector[Float], SparseVector[Float], SparseVector[Float]] =
  //        SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Float
  //
  //      override def valueAt(v: SparseVector[Float])(index: Int): Float =
  //        v(index)
  //
  //      override def toSeq(v: SparseVector[Float]): Seq[Float] =
  //        v.toArray.toSeq
  //
  //      override def map[B: ClassTag](v: SparseVector[Float])(f: (Float) => B): SparseVector[B] = {
  //        implicit val _0: Zero[B] = new Zero[B] { override val zero: B = null.asInstanceOf[B] }
  //        implicit val _1 = SparseVector.canMapValues[SparseVector[Float], B]
  //        v.map(f)
  //      }
  //
  //      override def size(v: SparseVector[Float]): Int =
  //        v.length
  //    }
  //
  //    object LongSparseMathVector extends MathVectorOps[Long, SparseVector] {
  //
  //      override def zeros(size: Int): SparseVector[Long] =
  //        SparseVector.zeros(size)
  //
  //      override def fill(size: Int)(value: Long): SparseVector[Long] =
  //        SparseVector.fill(size)(value)
  //
  //      override val addV: OpAdd.Impl2[SparseVector[Long], SparseVector[Long], SparseVector[Long]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Long_OpAdd
  //
  //      override val addS: OpAdd.Impl2[SparseVector[Long], Long, SparseVector[Long]] =
  //        new OpAdd.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
  //          override def apply(v: SparseVector[Long], v2: Long): SparseVector[Long] =
  //            v.map { _ + v2 }
  //        }
  //
  //      val subV: OpSub.Impl2[SparseVector[Long], SparseVector[Long], SparseVector[Long]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Long_OpSub
  //
  //      val subS: OpSub.Impl2[SparseVector[Long], Long, SparseVector[Long]] =
  //        new OpSub.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
  //          override def apply(v: SparseVector[Long], v2: Long): SparseVector[Long] =
  //            v.map { _ - v2 }
  //        }
  //
  //      override val dot: OpMulInner.Impl2[SparseVector[Long], SparseVector[Long], Long] =
  //        new OpMulInner.Impl2[SparseVector[Long], SparseVector[Long], Long] {
  //          override def apply(v: SparseVector[Long], v2: SparseVector[Long]): Long =
  //            v.dot(v2)
  //        }
  //
  //      override val divS: OpDiv.Impl2[SparseVector[Long], Long, SparseVector[Long]] =
  //        new OpDiv.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
  //          override def apply(v: SparseVector[Long], v2: Long): SparseVector[Long] =
  //            v.map { _ / v2 }
  //        }
  //
  //      override val mulS: OpMulScalar.Impl2[SparseVector[Long], Long, SparseVector[Long]] =
  //        new OpMulScalar.Impl2[SparseVector[Long], Long, SparseVector[Long]] {
  //          override def apply(v: SparseVector[Long], v2: Long): SparseVector[Long] =
  //            v.map { _ * v2 }
  //        }
  //
  //      override val divV: OpDiv.Impl2[SparseVector[Long], SparseVector[Long], SparseVector[Long]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Long_OpDiv
  //
  //      override val mulV: OpMulScalar.Impl2[SparseVector[Long], SparseVector[Long], SparseVector[Long]] =
  //        SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Long
  //
  //      override def valueAt(v: SparseVector[Long])(index: Int): Long =
  //        v(index)
  //
  //      override def toSeq(v: SparseVector[Long]): Seq[Long] =
  //        v.toArray.toSeq
  //
  //      override def map[B: ClassTag](v: SparseVector[Long])(f: (Long) => B): SparseVector[B] = {
  //        implicit val _0: Zero[B] = new Zero[B] { override val zero: B = null.asInstanceOf[B] }
  //        implicit val _1 = SparseVector.canMapValues[SparseVector[Long], B]
  //        v.map(f)
  //      }
  //
  //      override def size(v: SparseVector[Long]): Int =
  //        v.length
  //    }
  //
  //    object IntSparseMathVector extends MathVectorOps[Int, SparseVector] {
  //
  //      override def zeros(size: Int): SparseVector[Int] =
  //        SparseVector.zeros(size)
  //
  //      override def fill(size: Int)(value: Int): SparseVector[Int] =
  //        SparseVector.fill(size)(value)
  //
  //      override val addV: OpAdd.Impl2[SparseVector[Int], SparseVector[Int], SparseVector[Int]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Int_OpAdd
  //
  //      override val addS: OpAdd.Impl2[SparseVector[Int], Int, SparseVector[Int]] =
  //        new OpAdd.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
  //          override def apply(v: SparseVector[Int], v2: Int): SparseVector[Int] =
  //            v.map { _ + v2 }
  //        }
  //
  //      val subV: OpSub.Impl2[SparseVector[Int], SparseVector[Int], SparseVector[Int]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Int_OpSub
  //
  //      val subS: OpSub.Impl2[SparseVector[Int], Int, SparseVector[Int]] =
  //        new OpSub.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
  //          override def apply(v: SparseVector[Int], v2: Int): SparseVector[Int] =
  //            v.map { _ - v2 }
  //        }
  //
  //      override val dot: OpMulInner.Impl2[SparseVector[Int], SparseVector[Int], Int] =
  //        new OpMulInner.Impl2[SparseVector[Int], SparseVector[Int], Int] {
  //          override def apply(v: SparseVector[Int], v2: SparseVector[Int]): Int =
  //            v.dot(v2)
  //        }
  //
  //      override val divS: OpDiv.Impl2[SparseVector[Int], Int, SparseVector[Int]] =
  //        new OpDiv.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
  //          override def apply(v: SparseVector[Int], v2: Int): SparseVector[Int] =
  //            v.map { _ / v2 }
  //        }
  //
  //      override val mulS: OpMulScalar.Impl2[SparseVector[Int], Int, SparseVector[Int]] =
  //        new OpMulScalar.Impl2[SparseVector[Int], Int, SparseVector[Int]] {
  //          override def apply(v: SparseVector[Int], v2: Int): SparseVector[Int] =
  //            v.map { _ * v2 }
  //        }
  //
  //      override val divV: OpDiv.Impl2[SparseVector[Int], SparseVector[Int], SparseVector[Int]] =
  //        SparseVector.implOps_SVT_SVT_eq_SVT_Int_OpDiv
  //
  //      override val mulV: OpMulScalar.Impl2[SparseVector[Int], SparseVector[Int], SparseVector[Int]] =
  //        SparseVector.implOpMulScalar_SVT_SVT_eq_SVT_Int
  //
  //      override def valueAt(v: SparseVector[Int])(index: Int): Int =
  //        v(index)
  //
  //      override def toSeq(v: SparseVector[Int]): Seq[Int] =
  //        v.toArray.toSeq
  //
  //      override def map[B: ClassTag](v: SparseVector[Int])(f: (Int) => B): SparseVector[B] = {
  //        implicit val _0: Zero[B] = new Zero[B] { override val zero: B = null.asInstanceOf[B] }
  //        implicit val _1 = SparseVector.canMapValues[SparseVector[Int], B]
  //        v.map(f)
  //      }
  //
  //      override def size(v: SparseVector[Int]): Int =
  //        v.length
  //    }
  //  }

}

