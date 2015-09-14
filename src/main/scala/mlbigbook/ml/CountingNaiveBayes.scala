package mlbigbook.ml

import mlbigbook.wordcount.GenericCount

/**
 * Implementations of counting naive bayes for Int, Long, Float, and Double
 * numeric types.
 */
object CountingNaiveBayes {

  def apply[N: CountSmoothFactory]: CountingNaiveBayes[N] = {
    val sf = implicitly[CountSmoothFactory[N]]
    new CountingNaiveBayes[N] {
      override implicit val smoothFac = sf
    }
  }

  object ZeroCount {

    val Double = CountingNaiveBayes[Double](ZeroCsFactory.Implicits.doubleZsf)

    val Float = CountingNaiveBayes[Float](ZeroCsFactory.Implicits.floatZsf)

    val Long = CountingNaiveBayes[Long](ZeroCsFactory.Implicits.longZsf)

    val Int = CountingNaiveBayes[Int](ZeroCsFactory.Implicits.intZsf)
  }

  object Laplacian {

    val Double = CountingNaiveBayes[Double](ConstantSmoothFac.Laplacian.Implicits.doubleCsf)

    val Float = CountingNaiveBayes[Float](ConstantSmoothFac.Laplacian.Implicits.floatCsf)

    val Long = CountingNaiveBayes[Long](ConstantSmoothFac.Laplacian.Implicits.longCsf)

    val Int = CountingNaiveBayes[Int](ConstantSmoothFac.Laplacian.Implicits.intCsf)
  }

}

/**
 * Implementation of naive Bayes for discrete, event-based features.
 */
trait CountingNaiveBayes[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) N] {

  import NaiveBayesModule._

  /**
   * The factory that produces a count smoothing instance for the numeric type N.
   */
  implicit def smoothFac: CountSmoothFactory[N]

  /**
   * The numeric instance associated with this class's generic number parameter.
   */
  implicit final lazy val num: Numeric[N] = smoothFac.num

  //
  // Type Definitions
  //

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
   * Produces a naive Bayes model from the input data. Uses no count smoothing.
   */
  final def train[F, L](data: TrainingData[F, L]): NaiveBayes[F, L] = {
    val cs = count(data)
    val (labels, _, _) = cs
    val (prior, likelihood) = countsToPriorAndLikelihood(cs)
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
  final def countsToPriorAndLikelihood[F, L](c: Counts[L, F]): (LogPrior[L], LogLikelihood[F, L]) = {
    val (_, labelMap, featureMap) = c
    (mkPrior(labelMap), mkLikelihood(featureMap))
  }

  /**
   * Produces a prior function from a label mapping.
   */
  final def mkPrior[L](lm: LabelMap[L]): LogPrior[L] = {
    val totalClassCount = num.toDouble(lm.values.sum)
    val logPriorMap =
      lm.map {
        case (label, count) =>
          (
            label,
            math.log { num.toDouble(count) / totalClassCount }
          )
      }

    (label: L) =>
      if (logPriorMap contains label)
        logPriorMap(label)
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
  final def mkLikelihood[L, F](featureMap: FeatureMap[L, F]): LogLikelihood[F, L] = {

    val smoother = smoothFac(featureMap)

    val pseudoCount = num.toDouble(smoother.sCount)

    val notPresent =
      if (num.compare(smoother.sCount, num.zero) == 0)
        0.0
      else
        math.log(pseudoCount / num.toDouble(smoother.sAggregateTotal))

    // calculate likelihood for each (label,feature) pair
    val logLikelihoodMap = {
      featureMap.map {
        case (label, featureValues) =>

          val sTotalForLabel = num.toDouble(smoother.sAggregateLabel(label))
          (
            label,
            featureValues.map {
              case (feature, count) =>
                val dCount = num.toDouble(count)
                (
                  feature,
                  math.log { (dCount + pseudoCount) / sTotalForLabel }
                )
            }
          )
      }
    }

    // likelihood function
    (label: L) =>
      (feature: F) =>
        if (logLikelihoodMap contains label)
          logLikelihoodMap(label).getOrElse(feature, notPresent)
        else
          notPresent
  }

}