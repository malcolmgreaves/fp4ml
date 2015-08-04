package mlbigbook.ml

import mlbigbook.data._
import mlbigbook.ml.NaiveBayesModule.NaiveBayes
import mlbigbook.wordcount.NumericMap
import breeze.linalg._
import breeze.math._
import breeze.numerics._

import scala.reflect.ClassTag

object GaussianNaiveBayes {
  case object Double extends GaussianNaiveBayes[Double] {}
  case object Int extends GaussianNaiveBayes[Int] {}
  case object Long extends GaussianNaiveBayes[Long] {}
}

abstract class GaussianNaiveBayes[@specialized(scala.Double, scala.Long, scala.Int) N: Numeric]() {

  import breeze.linalg.Vector
  type Vec = Vector[N]

  final def produce[F: Equiv, L: Equiv](data: Learning[Vec, L]#TrainingData): NaiveBayes[Vec, L] = {

    ???
  }

  case class Gaussian(mean: Vec, variance: Vec)
  object Gaussian {
    implicit val ctN = ClassTag[N](implicitly[Numeric[N]].zero.getClass)
    implicit val storageN =
      new breeze.storage.Zero[N] {
        override def zero: N = implicitly[Numeric[N]].zero
      }
    val empty = Gaussian(Vector.zeros[N](0), Vector.zeros[N](0))
  }

  final def estimateGaussian[V[_] <: Vector[_]](data: Data[V[N]])(implicit sr: Semiring[V[N]], ct: ClassTag[V[N]]): Gaussian =
    data.take(1).headOption match {

      case Some(v) =>
        //        val nDimensions = v.size.toDouble
        //        val mean = {
        //          val s = data.reduce[V[N]] { case (a, b) => sr.+(a, b) }
        //          s.map(_ / nDimensions)
        //        }
        //        val variance = {
        //          ???
        //        }

        val (n_final, diff_ignored, mean_final, variance_final) =
          data
            .aggregate((0, sr.zero, sr.zero, sr.zero))(
              {
                case ((n, diff, mean, m2), next) =>
                  val newN = n + 1
                  //                    val delta: V[N] = sr.-(next, mean)
                  //                    val newMean: V[N] = sr.+(mean, delta.map( _ / newN))
                  val delta = XXX.add(next, mean)
                  val newMean = {
                    val newDelta = delta
                    XXX.add(mean, newDelta)
                  }
                  val newM2: V[N] = {
                    //                      val a: V[N] = next.-(mean)//sr.-(next, mean)
                    //                      val b: V[N] = sr.*(delta, a)
                    //                      val c: V[N] = sr.+(m2, b)
                    val a = XXX.subtract(next, mean)
                    val b = XXX.multiply(delta, a)
                    val c = XXX.add(m2, b)
                    c
                  }
                  (newN, delta, newMean, newM2)
              },
              {
                case ((n1, diff1, mean1, m2_1), (n2, diff2, mean2, m2_2)) =>
                  (
                    n1 + n2,
                    sr.+(diff1, diff2),
                    sr.+(mean1, mean2),
                    sr.+(m2_1, m2_2)
                  )
              }
            )

        val m: Vec = mean_final.asInstanceOf[Vec]
        val v: Vec = XXX.elemDivide(variance_final, implicitly[Numeric[N]].fromInt(n_final - 1)).asInstanceOf[Vec]
        Gaussian(m, v)

      case None =>
        Gaussian.empty
    }

}

object XXX {

  import breeze.linalg.Vector

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