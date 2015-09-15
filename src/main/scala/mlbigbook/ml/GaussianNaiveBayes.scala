package mlbigbook.ml

import mlbigbook.data._
import mlbigbook.ml.NaiveBayesModule.{ LogLikelihood, NaiveBayes }
import breeze.math._
import mlbigbook.wordcount.GenericCount

import scala.language.{ higherKinds, implicitConversions }
import scala.reflect.ClassTag

object GaussianNaiveBayes {

  def apply[N: Numeric]: GaussianNaiveBayes[N] = {
    val n = implicitly[Numeric[N]]
    new GaussianNaiveBayes[N] {
      override implicit def num: Numeric[N] = n
    }
  }

  object Instances {
    val Double = GaussianNaiveBayes[Double]
    val Float = GaussianNaiveBayes[Float]
    val Long = GaussianNaiveBayes[Long]
    val Int = GaussianNaiveBayes[Int]
  }
}

trait GaussianNaiveBayes[@specialized(scala.Double, scala.Long, scala.Int) N] {

  import NaiveBayesModule._

  implicit def num: Numeric[N]

  import breeze.linalg.Vector
  type Vec = Vector[N]

  def labelCount[L](data: TrainingData[_, L]): Map[L, Long] =
    data
      .aggregate(GenericCount.empty[L, Long])(
        {
          case (labelMap, (_, label)) =>
            GenericCount.increment(labelMap, label)
        },
        {
          case (lm1, lm2) =>
            GenericCount.combine(lm1, lm2)
        }
      )

  final def produce[L](data: TrainingData[Vec, L]): NaiveBayes[Vec, L] = {

    // count the occurrence of every label in the training data
    val labelMap = labelCount(data)

    // The arbitrary, but fixed, sequential ordering of the labels.
    val labels = labelMap.keys.toSeq

    // construct the prior
    val logPrior = mkPrior(labelMap)

    NaiveBayes(
      labels,
      logPrior,
      logLikelihood
    )
  }

  def mkLikelihood[L](data: TrainingData[Vec, L]): LogLikelihood[Vec, L] = {
    // estimate means and variances of all features
    // make likelihood according to these statistics + the
    // probability density function of a Gaussian distribution
    val likelihood: LogLikelihood[(N, Int), L] = {
      null
      //      val gau = estimateGaussian(data)
      //      val maxIndex = g.mean.size
      //      // TODO
      //      // (1) need to estimate (mean, variance) on a PER-CLASS basis
      //      // (2) make types work out here
      //      // (3) make everything work out with vector
      //      // (4) if cannot obtain (3), then replace with DenseVector everywhere (reasonable assumption for continuous features...)
      //      (label: L) => {
      //        val g = gau(label)
      //        {
      //          case (value, index) =>
      //            if(index >= 0 && index < maxIndex)
      //              (1.0 / math.sqrt(2.0 * math.pi * g.variance(index))) * math.exp((-0.5) * math.pow(value - g.mean(index), 2.0)/ g.variance(index))
      //            else
      //              0.0
      //        }
      //      }
    }
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

  import mlbigbook.math.XXX

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