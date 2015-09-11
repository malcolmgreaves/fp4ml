package mlbigbook.ml

import mlbigbook.wordcount.GenericCount

object CountingNaiveBayes {
  case object Int extends CountingNaiveBayes[Int]
  case object Long extends CountingNaiveBayes[Long]
  case object Float extends CountingNaiveBayes[Float]
  case object Double extends CountingNaiveBayes[Double]
}

abstract class CountingNaiveBayes[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) N: Numeric] {

  import NaiveBayesModule._

  //
  // Type Definitions
  //

  type Smoothing = N

  type SmoothedTrain[Feature, Label] = Smoothing => Train[Feature, Label]

  object SmoothedTrain {
    def apply[F: Equiv, L: Equiv]: SmoothedTrain[F, L] =
      s => td =>
        smoothedTrain[F, L](s)(td)(implicitly[Equiv[F]], implicitly[Equiv[L]])
  }

  object Train {
    def apply[F: Equiv, L: Equiv]: Train[F, L] =
      td =>
        train[F, L](td)(implicitly[Equiv[F]], implicitly[Equiv[L]])
  }

  type LabelMap[Label] = Map[Label, N]

  type FeatureMap[Label, Feature] = Map[Label, Map[Feature, N]]

  object FeatureMap {
    def empty[Label, Feature]: FeatureMap[Label, Feature] =
      Map.empty[Label, Map[Feature, N]]
  }

  type Counts[Label, Feature] = (Seq[Label], LabelMap[Label], FeatureMap[Label, Feature])

  //
  // Implementation
  //

  final val num: Numeric[N] = implicitly[Numeric[N]]

  //  def smoothedTrain[F:Equiv, L:Equiv](smooth: Smoothing): Train[F,L] =
  //    (data: Learning[Feature.Vector[F], L]#TrainingData) => {
  final def smoothedTrain[F: Equiv, L: Equiv](
    smooth: Smoothing
  )(
    data: TrainingData[F, L]
  ): NaiveBayes[F, L] = {
    val counts = count(data)
    val (labels, _, _) = counts
    val (prior, likelihood) = countsToPriorAndLikelihood(smooth, counts)
    NaiveBayes(
      labels,
      prior,
      likelihood
    )
  }

  final def train[F: Equiv, L: Equiv](data: TrainingData[F, L]): NaiveBayes[F, L] =
    smoothedTrain[F, L](num.zero)(data)

  final def count[Label: Equiv, F: Equiv](data: TrainingData[F, Label]): Counts[Label, F] = {
    val (lm, fm) =
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
    (lm.keys.toSeq, lm, fm)
  }

  final def countsToPriorAndLikelihood[F: Equiv, L](
    smooth: Smoothing,
    c:      Counts[L, F]
  ): (Prior[L], Likelihood[F, L]) = {
    val (_, labelMap, featureMap) = c
    (mkPrior(labelMap), mkLikelihood(smooth, featureMap))
  }

  final def mkPrior[L: Equiv](lm: LabelMap[L]): Prior[L] = {
    val totalClassCount = num.toDouble(lm.values.sum)
    val priormap =
      lm.map {
        case (label, count) =>
          (label, num.toDouble(count) / totalClassCount)
      }

    (label: L) =>
      if (priormap contains label)
        priormap(label)
      else
        0.0
  }

  final def mkLikelihood[L: Equiv, F: Equiv](
    smooth:     Smoothing,
    featureMap: FeatureMap[L, F]
  ): Likelihood[F, L] = {

    val totalFeatureClassCount: Double = {
      val totalSmoothPsuedocounts = {
        // TODO -- replace with bloom filter ! (and add 1 just to make sure that we get somethin'...)
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