package mlbigbook.ml

import mlbigbook.data._

object NaiveBayesModule {

  type Prior[Label] = Label => Dist[_]#Probability
  type Likelihood[Feature, Label] = Label => Feature => Dist[_]#Probability

  type FeatureVector[Feature] = DistData[Feature]

  trait NaiveBayes[Feature, Label] {
    def estimator: FeatureVector[Feature] => Dist[Label]
    val classifier: FeatureVector[Feature] => Label =
      (x: FeatureVector[Feature]) =>
        Argmax(estimator(x))
  }

  def apply[Feature, Label](
    labels: DistData[Label],
    p: Prior[Label],
    l: Likelihood[Feature, Label]
 ): NaiveBayes[Feature, Label] =
    (features: DistData[Feature]) =>
      DiscreteDist {
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


  def apply(nb: NaiveBayes[Feature, Label]):
}

sealed abstract class Dist[A] {

  type Item = A
  type Probability = Double
  type Density = Item => Probability

  def pdf: Density

  def range: Option[DistData[Item]]
}

case class DiscreteDist[A](m: Map[A, Dist[_]#Probability]) extends Dist[A] {

  override val pdf: Density =
    (x: Item) =>
      if (m contains x)
        m(x)
      else
        0.0

  import DistData._

  override lazy val range: Option[DistData[Item]] =
    Some(m.keys)
}

case class ContinuousDist[A](pdf: Dist[A]#Density) extends Dist[A] {
  override val range: Option[DistData[Item]] =
    None
}