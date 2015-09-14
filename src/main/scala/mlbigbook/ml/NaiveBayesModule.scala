package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag

object Feature {
  type Vector[F] = Data[F]
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
  type LogLikelihood[Feature, Label] = Label => Feature => LogProbability

  /**
   * An instance of naive Bayes, parametrized by a label set,
   * prior, and likelihood functions.
   */
  case class NaiveBayes[F, Label](
    labels: Data[Label],
    p:      LogPrior[Label],
    l:      LogLikelihood[F, Label]
  )

  // TODO -- Investigate this type class idea for NB. Good or bad idea?
  /*
  trait NaiveBayesTypeclass[NB, Feature, Label] {
    def labels(nb:NB):Data[Label]
    def prior(nb:NB):Prior[Label]
    def likelihood(nb:NB):Likelihood[Feature,Label]
  }
   */

  type TrainingData[F, Label] = Learning[Feature.Vector[F], Label]#TrainingData

  /**
   * The type for producing a NaiveBayes instance from a labeled data set (aka training).
   */
  type Train[F, Label] = TrainingData[F, Label] => NaiveBayes[F, Label]

  /**
   * Produces a discrete estimator from a learned NaiveBayes instance.
   */
  def apply[Feature: ClassTag, Label](nb: NaiveBayes[Feature, Label]): DiscreteEstimator[Feature, Label] =
    DiscreteEstimator[Feature, Label] {
      (features: Feature.Vector[Feature]) =>
        DiscreteDistribution {

          // calculate log-posterior distribution (across labels)
          val logPosteriors: Data[(Label, LogProbability)] =
            nb.labels
              .map { label =>
                val logPrior = nb.p(label)
                val logLikelihood = {
                  val labelLogLikelihood = nb.l(label)
                  features
                    .map(labelLogLikelihood)
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