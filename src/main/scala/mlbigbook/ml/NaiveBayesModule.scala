package mlbigbook.ml

import mlbigbook.data._

object Feature {
  type Vector[F] = Data[F]
}

object NaiveBayesModule {

  type Prior[Label] = Label => Distribution[_]#Probability
  type Likelihood[Feature, Label] = Label => Feature => Distribution[_]#Probability

  def apply[Feature, Label](
    labels: Data[Label],
    p: Prior[Label],
    l: Likelihood[Feature, Label]
  ): DiscreteEstimator[Feature, Label] =
    DiscreteEstimator[Feature, Label] {
      (features: Feature.Vector[Feature]) =>
        DiscreteDistribution {
          val logPosteriors =
            labels
              .map { label =>
                val labelLikelihood = l(label)
                (label, math.log(p(label)) + features.map(x => math.log(labelLikelihood(x))).sum)
              }

          val normalizationConstant = logPosteriors.map(_._2).reduce(_ + _)

          logPosteriors
            .map {
              case (label, logP) => (label, logP / normalizationConstant)
            }
            .toMap
        }
    }

}
