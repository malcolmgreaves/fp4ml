package mlbigbook.math

import breeze.math.Semiring
import breeze.linalg.{Vector, DenseVector}
import breeze.linalg.operators._
import breeze.storage.Zero
import mlbigbook.util
import spire.syntax.cfor._

import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

/**
  * Base partial implementation for DenseVectors. Implements the MathVectorOps
  * methods for the DenseVector type. Also defines the zeros, ones methds
  * of MathVectorOps.
  */
protected abstract class Dense[
    @specialized Num: Fractional: Zero: Semiring: ClassTag]
    extends BaseMathVecOps[Num, DenseVector] {

  override final def foreach[A](v: DenseVector[A])(f: A => Any): Unit =
    v.foreach(f)

  override final def zeros(size: Int): DenseVector[N] =
    DenseVector.zeros[N](size)

  override final def ones(size: Int): DenseVector[N] =
    DenseVector.ones[N](size)(
      implicitly[ClassTag[N]],
      implicitly[Semiring[N]]
    )

  override final def fill[A: ClassTag: Zero](size: Int)(value: => A) =
    DenseVector.fill(size)(value)

  override final def toSeq[A: ClassTag](v: DenseVector[A]): Seq[A] =
    util.copyToSeq(v.toArray)

  override final def size(v: DenseVector[_]): Int =
    v.length

  override final def apply[A](v: DenseVector[A])(index: Int): A =
    v(index)

  override final def map[B: ClassTag: Fractional: Zero](v: DenseVector[N])(
      f: N => B): DenseVector[B] =
    v.map(f)

  override final def reduce[B >: N: ClassTag](v: DenseVector[N])(r: (B,
                                                                     B) => B) =
    v.reduceLeft(r)

  override final def fold[B: ClassTag](v: DenseVector[N])(zero: B)(
      combine: (B, N) => B) =
    v.valuesIterator.foldLeft(zero)(combine)

  override final def copy(v: DenseVector[N]) = {
    val src = v.toArray
    val size = src.length
    val cpy = new Array[N](size)
    System.arraycopy(src, 0, cpy, 0, size)
    DenseVector(cpy)
  }
}

/**
  * Implementation for DenseVector[Double].
  */
object DoubleDenseMathVector extends Dense[Double] {
  override val addV = DenseVector.dv_dv_Op_Double_OpAdd
  override val addS = DenseVector.dv_s_Op_Double_OpAdd
  override val subV = DenseVector.dv_dv_Op_Double_OpSub
  override val subS = DenseVector.dv_s_Op_Double_OpSub
  override val dot =
    new OpMulInner.Impl2[DenseVector[Double], DenseVector[Double], Double] {

      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(b.length == a.length, "Vectors must be the same length!")
//      val boff =
//        if (b.stride >= 0) b.offset
//        else b.offset + b.stride * (b.length - 1)
//      val aoff =
//        if (a.stride >= 0) a.offset
//        else a.offset + a.stride * (a.length - 1)
//      BLAS.getInstance().sdot(
//        a.length, b.data, boff, b.stride, a.data, aoff, a.stride
//      )
        // TODO : Do we need to take into consideration ({a,b}.{stride,offset})
        //        into account here?
        var agg = 0.0
        cfor(0)(_ < a.length, _ + 1) { i =>
          agg += a(i) * b(i)
        }
        agg
      }

      implicitly[BinaryRegistry[Vector[Double],
                                Vector[Double],
                                OpMulInner.type,
                                Double]].register(this)
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
