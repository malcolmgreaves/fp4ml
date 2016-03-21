package mlbigbook.math

import breeze.math.Semiring
import breeze.linalg.{ Vector, DenseVector }
import breeze.linalg.operators._
import breeze.storage.Zero

import scala.language.{ higherKinds, implicitConversions }
import scala.reflect.ClassTag
/**
 * Base partial implementation for DenseVectors. Implements the MathVectorOps
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
    MathVectorOps.vecCopyToSeq(v.toArray)

  override def size(v: DenseVector[_]): Int =
    v.length

  override def valueAt[A](v: DenseVector[A])(index: Int): A =
    v(index)

  override def map[B: ClassTag: Numeric: Zero](v: DenseVector[N])(f: N => B): DenseVector[B] =
    v.map(f)

  override def reduce[A: ClassTag, Super >: A: ClassTag](v: DenseVector[A])(r: (Super, Super) => Super): Super =
    v.reduceLeft(r)

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