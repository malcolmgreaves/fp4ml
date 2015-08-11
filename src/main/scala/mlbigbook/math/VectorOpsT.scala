package mlbigbook.math

import breeze.linalg.{ NumericOps, DenseVector, Vector }
import breeze.linalg.operators._
import breeze.math.Semiring

import scala.language.implicitConversions

abstract class VectorOpsT[N: Numeric, V[_] <: Vector[_]] {

  implicit def zeros(size: Int): V[N]

  implicit def ones(size: Int): V[N]

  implicit val addV: OpAdd.Impl2[V[N], V[N], V[N]]

  implicit val addS: OpAdd.Impl2[V[N], N, V[N]]

  implicit val subV: OpSub.Impl2[V[N], V[N], V[N]]

  implicit val subS: OpSub.Impl2[V[N], N, V[N]]

  implicit val dot: OpMulInner.Impl2[V[N], V[N], N]

  implicit val mulV: OpMulScalar.Impl2[V[N], V[N], V[N]]

  implicit val mulS: OpMulScalar.Impl2[V[N], N, V[N]]

  implicit val divV: OpDiv.Impl2[V[N], V[N], V[N]]

  implicit val divS: OpDiv.Impl2[V[N], N, V[N]]

}

object VectorOpsT {

  implicit object DoubleDenseVectorOptsT extends VectorOpsT[Double, DenseVector] {

    override implicit def zeros(size: Int): DenseVector[Double] =
      DenseVector.zeros(size)

    override implicit def ones(size: Int): DenseVector[Double] =
      DenseVector.ones(size)

    override implicit val addV: OpAdd.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] =
      DenseVector.dv_dv_Op_Double_OpAdd

    override implicit val addS: OpAdd.Impl2[DenseVector[Double], Double, DenseVector[Double]] =
      DenseVector.dv_s_Op_Double_OpAdd

    implicit val subV: OpSub.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] =
      DenseVector.dv_dv_Op_Double_OpSub

    implicit val subS: OpSub.Impl2[DenseVector[Double], Double, DenseVector[Double]] =
      DenseVector.dv_s_Op_Double_OpSub

    override implicit val dot: OpMulInner.Impl2[DenseVector[Double], DenseVector[Double], Double] =
      DenseVector.canDotD

    override implicit val divS: OpDiv.Impl2[DenseVector[Double], Double, DenseVector[Double]] =
      DenseVector.dv_s_Op_Double_OpDiv

    override implicit val mulS: OpMulScalar.Impl2[DenseVector[Double], Double, DenseVector[Double]] =
      DenseVector.dv_s_Op_Double_OpMulScalar

    override implicit val divV: OpDiv.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] =
      DenseVector.dv_dv_Op_Double_OpDiv

    override implicit val mulV: OpMulScalar.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] =
      DenseVector.dv_dv_Op_Double_OpMulScalar

  }

}

object XXX {

  import breeze.linalg.Vector

  /*

  implicit object addAandB extends OpAdd.Impl2[A, B, Result] {
   def apply(a: A, b: B) = ...
}

   */

  def add_[N: Numeric, V[_] <: Vector[_]](v1: V[N], v2: V[N])(implicit no: NumericOps[V[N]]): V[N] = {

    ???
  }

  def addition[N: Numeric, V[_] <: Vector[_]](v1: V[N], v2: V[N])(implicit plus: OpAdd.Impl2[V[N], V[N], V[N]]) =
    plus(v1, v2)

  def add[N: Numeric, V[_] <: Vector[_]](v1: V[N], v2: V[N])(implicit sr: Semiring[V[N]]): V[N] =
    sr.+(v1, v2)

  def subtract[N: Numeric, V[_] <: Vector[_]](v1: V[N], v2: V[N])(implicit sr: Semiring[V[N]]): V[N] =
    ???
  //    sr.-(v1, v2)

  def multiply[N: Numeric, V[_] <: Vector[_]](v1: V[N], v2: V[N])(implicit sr: Semiring[V[N]]): V[N] =
    sr.*(v1, v2)

  def elemDivide[N: Numeric, V[_] <: Vector[_]](v: V[N], x: N)(implicit sr: Semiring[V[N]]): V[N] =
    ???
  //    v./=(x)

  //
  ////  def add[N:Numeric, V[_] <: Vector[_]](v1:V[N], v2:V[N])(implicit ops: NumericOps[V[N]]) = {
  //  def add[N:Numeric](v1: Vector[N], v2: Vector[N])(implicit ev: Semiring[Vector[N]]) = {
  ////  def add(v1: Vector[Double], v2: Vector[Double]) = {
  //    v1 match {
  //      case v1Dense: DenseVector[_] =>
  //        v2 match {
  //          case v2Dense: DenseVector[_] =>
  //            ev.+(v1Dense, v2Dense)
  ////            implicitly[Semiring[N]]
  ////            v1Dense + v2Dense
  ////            operators.OpAdd
  //
  //          case _ => ???
  //        }
  //      case _ => ???
  //    }
  //  }case

}

