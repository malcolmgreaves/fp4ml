package mlbigbook.ml

object CountSmoothing {

  trait SmoothFactory[N] {

    implicit def num: Numeric[N]

    def apply[Label, Feature](
      fm: CountingNaiveBayes[N]#FeatureMap[Label, Feature]
    ): Smooth[N, Label, Feature]
  }

  trait Smooth[N, Label, Feature] {

    implicit def num: Numeric[N]

    def smooth(l: Label)(f: Feature): N

    def aggregateLabel: Map[Label, N]

    def aggregateTotal: N
  }
}

