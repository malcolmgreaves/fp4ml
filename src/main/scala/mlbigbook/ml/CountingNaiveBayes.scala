package mlbigbook.ml

import mlbigbook.wordcount.GenericCount

/**
 * Implementations of counting naive bayes for Int, Long, Float, and Double
 * numeric types.
 */
object CountingNaiveBayes {
  case object Int extends CountingNaiveBayes[Int]
  case object Long extends CountingNaiveBayes[Long]
  case object Float extends CountingNaiveBayes[Float]
  case object Double extends CountingNaiveBayes[Double]
}

/**
 * Implementation of naive Bayes for discrete, event-based features.
 */
abstract class CountingNaiveBayes[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) N: Numeric] {

  import NaiveBayesModule._

  //
  // Type Definitions
  //

  /**
   * Type representing a smoothing value. It is a number.
   */
  type Smoothing = N

  /**
   * Type representing a training function that uses the given smoothing value
   * for "hallucinating" counts during training. This technique produces a more
   * robust naive Bayes model that is able to operate with events that it has
   * never encountered before.
   */
  type SmoothedTrain[Feature, Label] = Smoothing => Train[Feature, Label]

  /**
   * Helper object that allows one to construct an instance of
   * CountingNaiveBayes.SmoothedTrain using the smoothedTrain method defined
   * within the CountingNaiveBayes class.
   */
  object SmoothedTrain {
    def apply[F: Equiv, L: Equiv]: SmoothedTrain[F, L] =
      smoothedTrain[F, L]
  }

  /**
   * Helper object that allows one to construct an instance of
   * NaiveBayesModule.Train using the train method defined within the
   * CountingNaiveBayes class.
   */
  object Train {
    def apply[F: Equiv, L: Equiv]: Train[F, L] =
      train[F, L]
  }

  /**
   * Type representing the mapping between labels and the number of times each
   * label was encountered in a training data set.
   */
  type LabelMap[Label] = Map[Label, N]

  /**
   * Type representing the mapping between features and the number of times
   * each one was encountered during training. Each one of these maps is
   * partitioned by the label that was associated with the particular
   * observed feature-count co-occurrence.
   */
  type FeatureMap[Label, Feature] = Map[Label, Map[Feature, N]]

  object FeatureMap {
    /**
     * An empty feature map instance.
     */
    def empty[Label, Feature]: FeatureMap[Label, Feature] =
      Map.empty[Label, Map[Feature, N]]
  }

  /**
   * Type containing the raw count information produced during training.
   * The associated label sequence serve as the keys of the included label and
   * feature maps. In addition, this sequence provides a single ordering for
   * the labels. Note that this ordering is arbitrary, but fixed.
   */
  type Counts[Label, Feature] = (Seq[Label], LabelMap[Label], FeatureMap[Label, Feature])

  //
  // Implementations
  //

  /**
   * The numeric instance associated with this class's generic number parameter.
   */
  final val num: Numeric[N] = implicitly[Numeric[N]]

  /**
   * Produces a naive Bayes model from the input data. Uses no count smoothing.
   */
  final def train[F, L](data: TrainingData[F, L]): NaiveBayes[F, L] =
    smoothedTrain[F, L](num.zero)(data)

  /**
   * Produces a naive Bayes model from the input data using the given count
   * smoothing value.
   */
  final def smoothedTrain[F, L](smooth: Smoothing)(data: TrainingData[F, L]): NaiveBayes[F, L] = {
    val cs = count(data)
    val (labels, _, _) = cs
    val (prior, likelihood) = countsToPriorAndLikelihood(smooth, cs)
    NaiveBayes(
      labels,
      prior,
      likelihood
    )
  }

  /**
   * Collects co-occurrence counts across the input training data.
   */
  final def count[Label, F](data: TrainingData[F, Label]): Counts[Label, F] = {
    val (finalLabelMap, finalFeatureMap) =
      data
        .aggregate((GenericCount.empty[Label, N], FeatureMap.empty[Label, F]))(
          {
            case ((labelMap, featureMap), (features, label)) =>

              val updatedLabelMap = GenericCount.increment(labelMap, label)

              val existing = featureMap.getOrElse(label, GenericCount.empty[F, N])

              val updatedFeatureMapForLabel =
                features
                  .aggregate(existing)(
                    { case (fm, feature) => GenericCount.increment(fm, feature) },
                    GenericCount.combine[F, N]
                  )

              (updatedLabelMap, featureMap + (label -> updatedFeatureMapForLabel))
          },
          {
            case ((lm1, fm1), (lm2, fm2)) =>

              val combinedLabelMap = GenericCount.combine(lm1, lm2)

              val combinedFeatureMap =
                combinedLabelMap.keys
                  .map { label =>
                    (label, GenericCount.combine(fm1(label), fm2(label)))
                  }
                  .toMap

              (combinedLabelMap, combinedFeatureMap)
          }
        )
    (finalLabelMap.keys.toSeq, finalLabelMap, finalFeatureMap)
  }

  /**
   * Using the input smoothing value and counts generated from training data,
   * this method produces appropriate prior and likelihood functions.
   *
   * Uses the `mkPrior` and `mkLikelihood` methods.
   */
  final def countsToPriorAndLikelihood[F, L](
    smooth: Smoothing,
    c:      Counts[L, F]
  ): (Prior[L], Likelihood[F, L]) = {
    val (_, labelMap, featureMap) = c
    (mkPrior(labelMap), mkLikelihood(smooth, featureMap))
  }

  /**
   * Produces a prior function from a label mapping.
   */
  final def mkPrior[L](lm: LabelMap[L]): Prior[L] = {
    val totalClassCount = num.toDouble(lm.values.sum)
    val priorMap =
      lm.map {
        case (label, count) =>
          (label, num.toDouble(count) / totalClassCount)
      }

    (label: L) =>
      if (priorMap contains label)
        priorMap(label)
      else
        0.0
  }

  /**
   * Produces a likelihood function from a feature mapping. Every feature-label
   * co-occurrence count has an added "hallucinated" count, which is equal to
   * input the smooth parameter's value. These modified co-occurrence counts
   * are used to estimate probabilities as well as come up with a pseudo count.
   * When applying the resulting Likelihood function to a feature and label
   * combination that was not observed during training, the function will
   * evaluate to this pseudo count (instead of zero).
   */
  final def mkLikelihood[L, F](
    smooth:     Smoothing,
    featureMap: FeatureMap[L, F]
  ): Likelihood[F, L] = {

    val totalFeatureClassCount: Double = {
      val totalSmoothPsuedocounts = {
        val nDistinctFeatures =
          featureMap
            .map(_._2.keySet)
            .reduce(_ ++ _)
            .size
            .toDouble
        nDistinctFeatures * num.toDouble(smooth)
      }
      num.toDouble(featureMap.map(_._2.values.sum).sum) + totalSmoothPsuedocounts
    }

    val s = num.toDouble(smooth)
    val pseudoCount = s / totalFeatureClassCount

    val likelihoodMap =
      featureMap.map {
        case (label, featMap) =>
          (
            label,
            featMap.map {
              case (feature, count) =>
                (feature, (num.toDouble(count) + s) / totalFeatureClassCount)
            }
          )
      }

    // likelihood function signature & implementation (finally!)
    (label: L) =>
      (feature: F) =>
        if (likelihoodMap contains label) {
          val fmap = likelihoodMap(label)
          if (fmap contains feature)
            fmap(feature)
          else
            pseudoCount
        } else {
          pseudoCount
        }
  }

}