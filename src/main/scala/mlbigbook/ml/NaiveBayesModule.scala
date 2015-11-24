package mlbigbook.ml

import mlbigbook.data._

import scala.language.implicitConversions
import scala.reflect.ClassTag

object Feature {
  trait Vector[F, N] {
    implicit def num: Numeric[N]
    def data: DataClass[(F, N)]
  }

  object Vector {

    object Implicits {

      implicit def from[F, N: Numeric](d: DataClass[(F, N)]): Vector[F, N] = {
        val n = implicitly[Numeric[N]]
        new Vector[F, N] {
          override final lazy val num = n
          override final val data = d
        }
      }
    }
  }

}

object NaiveBayesModule {

  /**
   * This module uses a log probabilities instead of probabilities.
   */
  type LogProbability = Double

  /**
   * A prior function. The estimated probability of a label.
   */
  type LogPrior[Label] = Label => LogProbability

  /**
   * A likelihood function. Conditioned on a label, produces an
   * estimated probability for a feature.
   */
  type LogLikelihood[Feature, Label, N] = Label => (Feature, N) => LogProbability

  /**
   * An instance of naive Bayes, parametrized by a label set,
   * prior, and likelihood functions.
   */
  case class NaiveBayes[F, Label, N](
    labels: DataClass[Label],
    p:      LogPrior[Label],
    l:      LogLikelihood[F, Label, N]
  )

  /**
   * Type representing the mapping between labels and the number of times each
   * label was encountered in a training data set.
   */
  type LabelMap[Label] = Map[Label, Long]

  /**
   * Type representing training data that naive bayes implementations use.
   */
  type TrainingData[F, Label, N] = Learning[Feature.Vector[F, N], Label]#TrainingData

  /**
   * The type for producing a NaiveBayes instance from a labeled data set (aka training).
   */
  type Train[F, Label, N] = TrainingData[F, Label, N] => NaiveBayes[F, Label, N]

  /**
   * Produces a prior function from a label mapping.
   */
  final def mkPrior[L](lm: LabelMap[L]): LogPrior[L] = {
    val totalLabelCount = lm.values.sum.toDouble
    val logPriorMap =
      lm.map {
        case (label, count) =>
          (
            label,
            math.log { count.toDouble / totalLabelCount }
          )
      }

    (label: L) =>
      logPriorMap.getOrElse(label, 0.0)
  }

  /**
   * Produces a discrete estimator from a learned NaiveBayes instance.
   */
  def apply[Feature: ClassTag, Label, N: Numeric](
    nb: NaiveBayes[Feature, Label, N]
  ) =
    DiscreteEstimator[Feature, Label, N] {
      (features: Feature.Vector[Feature, N]) =>
        DiscreteDistribution {

          // calculate log-posterior distribution (across labels)
          val logPosteriors: DataClass[(Label, LogProbability)] =
            nb.labels
              .map { label =>
                val logPrior = nb.p(label)
                val logLikelihood = {
                  val labelLogLikelihood = nb.l(label)
                  features.data
                    .map(labelLogLikelihood.tupled)
                    .sum
                }
                (label, logPrior + logLikelihood)
              }

          // to produce valid probabilities, we normalize each log-posterior
          val normalizationConstant =
            logPosteriors
              .map {
                case (_, logP) => logP
              }
              .reduce(_ + _)

          logPosteriors
            .map {
              case (label, logP) => (label, logP / normalizationConstant)
            }
            .toMap
        }
    }

}