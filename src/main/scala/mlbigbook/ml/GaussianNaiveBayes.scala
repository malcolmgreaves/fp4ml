package mlbigbook.ml

import mlbigbook.wordcount.GenericCount

import scala.language.implicitConversions

object GaussianNaiveBayes {

  def apply[N: GaussianFactory]: GaussianNaiveBayes[N] = {
    val g = implicitly[GaussianFactory[N]]
    new GaussianNaiveBayes[N] {
      override implicit val gauFac = g
    }
  }

  object Instances {
    import GaussianFactory.Implicits._
    val Double = GaussianNaiveBayes[Double]
    val Float = GaussianNaiveBayes[Float]
  }
}

trait GaussianNaiveBayes[@specialized(scala.Double, scala.Long, scala.Int) N] {

  import NaiveBayesModule._

  implicit val gauFac: GaussianFactory[N]

  implicit lazy val num = gauFac.num

  def labelCount[L](data: TrainingData[_, L, N]): Map[L, Long] =
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

  final def produce[F, L](data: TrainingData[F, L, N]): NaiveBayes[F, L, N] = {

    // count the occurrence of every label in the training data
    val labelMap = labelCount(data)

    // The arbitrary, but fixed, sequential ordering of the labels.
    val labels = labelMap.keys.toSeq

    // construct the prior function
    val logPrior = mkPrior(labelMap)

    // construct the likelihood function
    val logLikelihood = mkLikelihood(labelMap, data)

    NaiveBayes(
      labels,
      logPrior,
      logLikelihood
    )
  }

  def mkLikelihood[F, L](labelMap: LabelMap[L], data: TrainingData[F, L, N]): LogLikelihood[F, L, N] = {

    val cardinality =
      data
        .aggregate(0l)(
          {
            case (maxSize, (instance, _)) =>
              math.max(maxSize, instance.data.size)
          },
          math.max
        )
        .toInt

    val estGauByLabel =
      labelMap
        .map {
          case (label, _) =>
            val onlyWithLabel =
              data
                .filter {
                  case (_, instanceLabel) => label == instanceLabel
                }
                .map {
                  case (instance, _) => instance
                }

            (
              label,
              gauFac(cardinality, onlyWithLabel)
            )
        }

    val defaultGauByLabel =
      estGauByLabel
        .map {
          case (label, fgMap) =>
            (
              label,
              gauFac.Gaussian(
                mean = fgMap.map(_._2.mean).sum,
                variance = fgMap.map(_._2.variance).sum,
                stddev = fgMap.map(_._2.stddev).sum
              )
            )
        }

    val defaultGauAcrossLabel = {
      val (fMean, fVariance, fStddev) =
        estGauByLabel
          .foldLeft((num.zero, num.zero, num.zero)) {
            case ((mean, variance, stddev), (_, fgMap)) =>
              (
                num.plus(mean, fgMap.map(_._2.mean).sum),
                num.plus(variance, fgMap.map(_._2.variance).sum),
                num.plus(stddev, fgMap.map(_._2.stddev).sum)
              )
          }
      new gauFac.Gaussian(
        mean = fMean,
        variance = fVariance,
        stddev = fStddev
      )
    }

    (label: L) =>
      (feature: F, value: N) =>
        num.toDouble {
          if (estGauByLabel contains label) {
            val labelGau = estGauByLabel(label)

            val featGau =
              if (labelGau contains feature)
                labelGau(feature)
              else
                defaultGauByLabel(label)

            gauFac.logProbabilityOf(featGau)(value)

          } else
            gauFac.logProbabilityOf(defaultGauAcrossLabel)(value)
        }

  }

}