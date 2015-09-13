package mlbigbook.ml

import mlbigbook.ml.CountSmoothing.{ Smooth, SmoothFactory }

trait ConstantSmoothing[N] extends SmoothFactory[N] { factory =>

  def pseudoCount: N

  override def apply[Label, Feature](
    featureMap: CountingNaiveBayes[N]#FeatureMap[Label, Feature]
  ): Smooth[N, Label, Feature] =
    new Smooth[N, Label, Feature] {

      override implicit val num = factory.num

      override val aggregateLabel =
        featureMap
          .map {
            case (label, featureValues) =>
              val total = featureValues.values.sum
              val nDistinctFeats = num.fromInt(featureValues.keySet.size)
              (
                label,
                num.plus(
                  total,
                  num.times(nDistinctFeats, pseudoCount)
                )
              )
          }

      override val aggregateTotal: N =
        aggregateLabel.values.sum

      override def smooth(l: Label)(f: Feature): N =
        if (featureMap contains l)
          featureMap(l).getOrElse(f, pseudoCount)
        else
          pseudoCount
    }
}