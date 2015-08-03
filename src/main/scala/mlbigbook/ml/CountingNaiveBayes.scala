package mlbigbook.ml

import mlbigbook.data._
import mlbigbook.wordcount.NumericMap

object CountingNaiveBayes {
  case object Int extends CountingNaiveBayes[Int] {}
  case object Long extends CountingNaiveBayes[Long] {}
}

sealed abstract class CountingNaiveBayes[@specialized(scala.Int, scala.Long) N: Numeric]() {

  import NaiveBayesModule._

  implicit val nm = NumericMap[N]

  type Smoothing = N

  def produce[F: Equiv, L: Equiv](
    data: Learning[Feature.Vector[F], L]#TrainingData,
    smooth: Smoothing = implicitly[Numeric[N]].one): NaiveBayes[F, L] = {
    val (labelMap, featureMap) = count(data)
    println(s"label map:\n$labelMap\n")
    println(s"feature map:\n$featureMap\n")

    val (prior, likelihood) = counts2priorandlikeihood((labelMap, featureMap), smooth)
    NaiveBayes(
      labelMap.keys.toSeq,
      prior,
      likelihood
    )
  }

  // count features
  // incorporate smoothing
  // for each vector, v:
  //    c = v.class()                                             assert(shouldBeNeg.)
  //    for each feature, f:
  //      if v(f) is nonzero:
  //        featurevalu
  // by class
  //    classcount[c] += 1
  //
  // likelihood[c][f] = LOG ( featurevaluecount[c][f] / sum c' { featurevalucecount[c'][f] } )
  // posterior[c] = LOG ( classcount[c] / sum c' { classcount[c'] } )
  //
  // for new one:
  //    for each feature *f* in new one:
  //      s += likelihood[c'][*f*]
  //    posterior[c'] + s

  type LabelMap[Label] = NumericMap[N]#M[Label]

  type FeatureMap[Label, Feature] = Map[Label, NumericMap[N]#M[Feature]]

  object FeatureMap {
    def empty[Label, Feature]: FeatureMap[Label, Feature] =
      Map.empty[Label, NumericMap[N]#M[Feature]]
  }

  type Counts[Label, Feature] = (LabelMap[Label], FeatureMap[Label, Feature])

  def count[Label: Equiv, F: Equiv](data: Data[(Feature.Vector[F], Label)])(implicit nm: NumericMap[N]): Counts[Label, F] =
    data
      .aggregate((nm.empty[Label], FeatureMap.empty[Label, F]))(
        {
          case ((labelMap, featureMap), (features, label)) =>

            val updatedLabelMap = nm.increment(labelMap, label)

            val existing = featureMap.getOrElse(label, nm.empty[F])

            val updatedFeatureMapForLabel =
              features
                .aggregate(existing)(
                  { case (fm, feature) => nm.increment(fm, feature) },
                  nm.combine
                )

            (updatedLabelMap, featureMap + (label -> updatedFeatureMapForLabel))
        },
        {
          case ((lm1, fm1), (lm2, fm2)) =>

            val combinedLabelMap = nm.combine(lm1, lm2)

            val combinedFeatureMap =
              combinedLabelMap.keys
                .map { label =>
                  (label, nm.combine(fm1(label), fm2(label)))
                }
                .toMap

            (combinedLabelMap, combinedFeatureMap)
        }
      )

  def counts2priorandlikeihood[F: Equiv, L](
    c: Counts[L, F],
    smooth: Smoothing = implicitly[Numeric[N]].one): (Prior[L], Likelihood[F, L]) = {

    val num = implicitly[Numeric[N]]
    val (labelMap, featureMap) = c

    val prior = {
      val totalClassCount = num.toDouble(labelMap.map(_._2).sum)
      labelMap.map {
        case (label, count) =>
          (label, math.log(num.toDouble(count) / totalClassCount))
      }
    }

    val likelihood = {

      val totalSmoothPsuedocounts = {
        // todo -- replace with bloom filter ! (and add 1 just to make sure that we get somethin'...)
        val nDistinctFeatures =
          featureMap
            .map(_._2.keySet)
            .reduce(_ ++ _)
            .size
            .toDouble
        nDistinctFeatures * num.toDouble(smooth)
      }

      val totalFeatureClassCount =
        num.toDouble(featureMap.map(_._2.map(_._2).sum).sum) + totalSmoothPsuedocounts

      val smoother = {
        val s = num.toDouble(smooth)
        (x: N) => num.toDouble(x) + s
      }

      featureMap.map {
        case (label, featMap) =>
          (
            label,
            featMap.map {
              case (feature, count) =>
                (feature, math.log(smoother(count) / totalFeatureClassCount))
            }
          )
      }
    }

    (prior, likelihood)
  }

}