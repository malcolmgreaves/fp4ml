package mlbigbook.ml

import mlbigbook.ml.CountSmoothing.{ SmoothFactory, Smooth }

trait NoSmoothing[N] extends SmoothFactory[N] { factory =>

  val zero = num.zero

  override def apply[Label, Feature](fm: CountingNaiveBayes[N]#FeatureMap[Label, Feature]): Smooth[N, Label, Feature] =
    new Smooth[N, Label, Feature] {

      override implicit val num = factory.num

      override val aggregateLabel =
        fm.map {
          case (label, featureValues) =>
            (label, featureValues.values.sum)
        }

      override val aggregateTotal: N =
        aggregateLabel.values.sum

      override def smooth(l: Label)(f: Feature): N =
        if (fm contains l)
          fm(l).getOrElse(f, zero)
        else
          zero
    }
}

object NoSmoothing {

  object Implicits {

    implicit object DoubleNs extends NoSmoothing[Double] {
      override implicit val num = implicitly[Numeric[Double]]
    }

    implicit object FloatNs extends NoSmoothing[Float] {
      override implicit val num = implicitly[Numeric[Float]]
    }

    implicit object LongNs extends NoSmoothing[Long] {
      override implicit val num = implicitly[Numeric[Long]]
    }

    implicit object IntNs extends NoSmoothing[Int] {
      override implicit val num = implicitly[Numeric[Int]]
    }
  }
}