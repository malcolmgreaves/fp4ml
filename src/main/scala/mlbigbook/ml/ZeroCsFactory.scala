package mlbigbook.ml

trait ZeroCsFactory[N] extends CountSmoothFactory[N] {

  override def apply[Label, Feature](
    fm: CountingNaiveBayes[N]#FeatureMap[Label, Feature]
  ): Smoothing[Label, Feature] =
    new Smoothing[Label, Feature] {

      override val sCount =
        num.zero

      override val sAggregateLabel =
        fm.map {
          case (label, featureValues) =>
            (label, featureValues.values.sum)
        }

      override lazy val sAggregateTotal =
        sAggregateLabel.values.sum
    }
}

object ZeroCsFactory {

  def apply[N: Numeric]: ZeroCsFactory[N] = {
    val n = implicitly[Numeric[N]]
    new ZeroCsFactory[N] {
      override implicit val num: Numeric[N] = n
    }
  }

  object Implicits {

    implicit val doubleZsf = ZeroCsFactory[Double]

    implicit val floatZsf = ZeroCsFactory[Float]

    implicit val longZsf = ZeroCsFactory[Long]

    implicit val intZsf = ZeroCsFactory[Int]
  }
}