package mlbigbook.math

import breeze.linalg.SparseVector
import breeze.linalg.operators._
import breeze.math.Semiring
import breeze.storage.Zero
import spire.syntax.cfor._

import scala.language.higherKinds
import scala.reflect.ClassTag

/**
 * Base partial implementation for DenseVectors. Implements the MathVectorOps
 * methods for the DenseVector type. Also defines the zeros, ones methds
 * of MathVectorOps.
 */
protected abstract class Sparse[@specialized Num: Fractional: Zero: Semiring: ClassTag]
    extends BaseMathVecOps[Num, SparseVector] {

  override final def foreach[A](v: SparseVector[A])(f: A => Any) =
    v.foreach(f)

  override final def zeros(size: Int) =
    SparseVector.zeros[N](size)

  override final def ones(size: Int) =
    SparseVector.fill(size)(one)

  override final def fill[A: ClassTag: Zero](size: Int)(value: => A) =
    SparseVector.fill(size)(value)

  override final def toSeq[A: ClassTag](v: SparseVector[A]) = {
    val values = new Array[A](v.length)
    cfor(0)(_ < values.length, _ + 1){ i =>
      values(i) = v(i)
    }
    values.toSeq
  }

  override final def size(v: SparseVector[_]): Int =
    v.length

  override final def apply[A](v: SparseVector[A])(index: Int) =
    v(index)

  import SparseVector._

  override final def map[B: ClassTag: Fractional: Zero](v: SparseVector[N])(f: N => B) =
    v.map(f)

  override final def reduce[A1 >: N: ClassTag](v: SparseVector[N])(r: (A1, A1) => A1) =
    v.reduceLeft(r)

  override final def fold[B: ClassTag](v: SparseVector[N])(zero: B)(combine: (B, N) => B) =
    v.valuesIterator
      .foldLeft(zero)(combine)

  override final def copy(v: SparseVector[N]) =
    v.copy
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