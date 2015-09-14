package mlbigbook.ml

trait CountSmoothFactory[N] { factory =>

  implicit val num: Numeric[N]

  def apply[Label, Feature](
    fm: CountingNaiveBayes[N]#FeatureMap[Label, Feature]
  ): Smoothing[Label, Feature]

  trait Smoothing[Label, Feature] {

    implicit final val num: Numeric[N] = factory.num

    def sCount: N

    def sAggregateLabel: Map[Label, N]

    def sAggregateTotal: N
  }

}