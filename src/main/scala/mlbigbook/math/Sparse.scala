package mlbigbook.math

import breeze.linalg.SparseVector
import breeze.linalg.operators._
import breeze.math.Semiring
import breeze.storage.Zero

import scala.reflect.ClassTag

/**
 * Base partial implementation for DenseVectors. Implements the MathVectorOps
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
    MathVectorOps.vecCopyToSeq(v.toDenseVector.toArray)

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