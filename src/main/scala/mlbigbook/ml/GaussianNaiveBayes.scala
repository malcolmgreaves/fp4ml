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

  final def produce[L](data: TrainingData[Vec, L])(implicit sr: Semiring[Vec]): NaiveBayes[Vec, L] = {

    // count the occurrence of every label in the training data
    val labelMap = labelCount(data)

    // The arbitrary, but fixed, sequential ordering of the labels.
    val labels = labelMap.keys.toSeq

    // construct the prior function
    val logPrior = mkPrior(labelMap)

    // construct the likelihood function
    val logLikelihood = mkLikelihood(labelMap, fixDataType(data))

    NaiveBayes(
      labels,
      logPrior,
      logLikelihood
    )
  }

  private[this] def fixDataType[L](d: TrainingData[Vec, L]): Data[(Vec, L)] =
    ???

  def mkLikelihood[L](labelMap: LabelMap[L], data: Data[(Vec, L)])(implicit sr: Semiring[Vec]): LogLikelihood[Vec, L] = {
    // estimate means and variances of all features
    // make likelihood according to these statistics + the
    // probability density function of a Gaussian distribution
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


    val m1 =
      labelMap
        .map {
          case (estimatingForLabel, _) =>

            val vectorsWithLabel: Data[Vec] =
              data
                .filter {
                  case (_, label) => estimatingForLabel == label
                }
                .map {
                  case (instance, _) => instance
                }

            val gaussianForLabel = estimateGaussian(vectorsWithLabel)

            (estimatingForLabel, gaussianForLabel)
        }

    val defaultGau = estimateGaussian(data.map(_._1))


    (label: L) =>
      (feature: Vec) => {

        val resultingVec =
          if(m1 contains label)
            Gaussian.logProbability(m1(label))(feature)
          else
            Gaussian.logProbability(defaultGau)(feature)

          resultingVec
            .map(num.toDouble)
            .foldLeft(0.0) {
              case (accum, value) =>
                accum + value
            }
      }
  }

  /*

  trait GaussianFactory[N] { factory =>

    implicit def num: Numeric[N]

    def dimensionality: Int

    type Vec <: Vector[N]

    lazy val pi = Vec.fill(dimensionality)(math.Pi)

    lazy val ones = Vec.ones(dimensionality)

    lazy val twos = Vec.fill(dimensionality)(2.0)

    lazy val negOneHalf = Vec.fill(dimensionality)(-0.5)

    def apply[Label](d: Data[(Vec, Label)]): Gaussian

    case class Gaussian(
      mean: Vec,
      variance: Vec,
      stddev: Vec
    ){
      implicit final val num   = factory.num
      final val dimensionality = factory.dimensionality
    }

    /*** [BEGIN] MATH OPS ***/

    def sqrt(v: Vec): Vec = ???

    def e(v: Vec): Vec = ???

    def pow(base: Vec, exponent: Vec): Vec = ???

    /*** [END] MATH OPS ***/

    def probabilityOf(gau: Gaussian)(value: Vec): Vec = {
     // (1 / ( sqrt ( 2 * pi * stddev^2 ) ) ^ ( e^ (  -(1/2) * (  ( VALUE - mean )  /  stddev  ) )^2  )

     val base =
      ( ones / sqrt(twos * pi * gau.variance) )

      val exponent = {
        val eExponent = {

          val rightPart =
            pow(( (value - gau.mean) / gau.stddev ), twos)

          negOneHalf * rightPart
        }

        e(eExponent)
      }

      pow(base, exponent)
   }

   def logProbabilityOf(gau: Gaussian)(value: Vec) =
    probabilityOf(gau)(value)
      .map(math.log)

  }


 */

  case class Gaussian(mean: Vec, variance: Vec)
  object Gaussian {

    implicit val ctN = ClassTag[N](num.zero.getClass)

    implicit val storageN =
      new breeze.storage.Zero[N] {
        override def zero: N = num.zero
      }

    val empty = Gaussian(Vector.zeros[N](0), Vector.zeros[N](0))

    def logProbability(g: Gaussian)(v: Vec): Vec = ???
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