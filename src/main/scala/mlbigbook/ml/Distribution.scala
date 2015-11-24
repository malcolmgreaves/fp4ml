package mlbigbook.ml

import mlbigbook.data.DataClass

object Distribution {
  type Probability = Double
  type Density[Item] = Item => Probability
  type Range[Item] = Option[DataClass[Item]]
}

sealed abstract class Distribution[A] {

  type Item = A

  val pdf: Distribution.Density[Item]

  val range: Distribution.Range[Item]
}

case class DiscreteDistribution[A](m: Map[A, Distribution.Probability]) extends Distribution[A] {

  override val pdf =
    (x: Item) =>
      if (m contains x)
        m(x)
      else
        0.0

  override val range: Distribution.Range[A] =
    Some(m.keys)

  def toSeq: Seq[(A, Distribution.Probability)] =
    m.toSeq
}

case class ContinuousDistribution[A](pdf: Distribution.Density[A]) extends Distribution[A] {
  override val range: Option[DataClass[Item]] =
    None
}