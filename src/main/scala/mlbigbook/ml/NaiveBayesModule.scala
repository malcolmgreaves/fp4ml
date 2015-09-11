package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag

object Feature {
  type Vector[F] = Data[F]
}

object NaiveBayesModule {

  /**
   * This module uses a Distribtuion's notion of probability.
   */
  type Probability = Distribution.Probability

  /**
   * A prior function. The estimated probability of a label.
   */
  type Prior[Label] = Label => Probability

  /**
   * A likelihood function. Conditioned on a label, produces an
   * estimated probability for a feature.
   */
  type Likelihood[Feature, Label] = Label => Feature => Probability

  /**
   * An instance of naive Bayes, parametrized by a label set,
   * prior, and likelihood functions.
   */
  case class NaiveBayes[F, Label](
    labels: Data[Label],
    p:      Prior[Label],
    l:      Likelihood[F, Label]
  )

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
          val logPosteriors: Data[(Label, Probability)] =
            nb.labels
              .map { label =>
                val logPrior = math.log(nb.p(label))
                val logLikelihood = {
                  val labelLogLikelihood = nb.l(label)
                  features
                    .map(x => math.log(labelLogLikelihood(x)))
                    .sum
                }
                (label, logPrior + logLikelihood)
              }

          // to produce valid probabilities, we normalize each log-posterior
          val normalizationConstant =
            logPosteriors
              .map(_._2)
              .reduce(_ + _)

          logPosteriors
            .map {
              case (label, logP) => (label, logP / normalizationConstant)
            }
            .toMap
        }
    }

}