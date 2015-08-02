package mlbigbook.ml

import mlbigbook.data._

object Feature {
  type Vector[F] = Data[F]
}

object NaiveBayesModule {

  type Prior[Label] = Label => Distribution[_]#Probability
  type Likelihood[Feature, Label] = Label => Feature => Distribution[_]#Probability

  case class NaiveBayes[F, Label](
    labels: Data[Label],
    p: Prior[Label],
    l: Likelihood[Feature.Vector[F], Label])

  type Produce[F, Label] = Learning[Feature.Vector[F], Label]#TrainingData => NaiveBayes[F, Label]

  def apply[Feature, Label](nb: NaiveBayes[Feature, Label]): DiscreteEstimator[Feature, Label] =
    DiscreteEstimator[Feature, Label] {
      (features: Feature.Vector[Feature]) =>
        DiscreteDistribution {
          val logPosteriors =
            nb.labels
              .map { label =>
                val labelLikelihood = nb.l(label)
                (label, math.log(nb.p(label)) + features.map(x => math.log(labelLikelihood(x))).sum)
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
