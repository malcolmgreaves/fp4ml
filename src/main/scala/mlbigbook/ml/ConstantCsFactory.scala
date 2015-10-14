package mlbigbook.ml

trait ConstantCsFactory[N] extends CountSmoothFactory[N] { factory =>

  val pseudoCount: N

  override def apply[Label, Feature](
    featureMap: CountingNaiveBayes[N]#FeatureMap[Label, Feature]
  ): Smoothing[Label, Feature] =
    new Smoothing[Label, Feature] {

      override val sCount =
        pseudoCount

      override val sAggregateLabel =
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

      override val sAggregateTotal: N =
        sAggregateLabel.values.sum
    }
}

object ConstantCsFactory {

  def apply[N: Numeric](pseudoCount: N): ConstantCsFactory[N] = {
    val p = pseudoCount
    val n = implicitly[Numeric[N]]
    new ConstantCsFactory[N] {
      override val pseudoCount: N = p
      override implicit val num: Numeric[N] = n
    }
  }

  object Laplacian {

    def apply[N: Numeric]: ConstantCsFactory[N] =
      ConstantCsFactory[N](implicitly[Numeric[N]].one)

    object Implicits {

      implicit val doubleCsf = Laplacian[Double]

      implicit val floatCsf = Laplacian[Float]

      implicit val longCsf = Laplacian[Long]

      implicit val intCsf = Laplacian[Int]
    }
  }
}