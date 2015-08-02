package mlbigbook.ml

import mlbigbook.data.Data

sealed abstract class Distribution[A] {

  type Item = A
  type Probability = Double
  type Density = Item => Probability

  def pdf: Density

  def range: Option[Data[Item]]
}

case class DiscreteDistribution[A](m: Map[A, Distribution[_]#Probability]) extends Distribution[A] {

  override val pdf: Density =
    (x: Item) =>
      if (m contains x)
        m(x)
      else
        0.0

  import Data._

  override def range: Option[Data[Item]] =
    Some(m.keys)

  def toSeq: Seq[(A, Distribution[_]#Probability)] =
    m.toSeq
}

case class ContinuousDistribution[A](pdf: Distribution[A]#Density) extends Distribution[A] {
  override val range: Option[Data[Item]] =
    None
}