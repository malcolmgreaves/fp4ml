package mlbigbook.ml

import mlbigbook.data._
import mlbigbook.wordcount.NumericMap

import scala.reflect.ClassTag

abstract class NaiveBayesModule {

  type Probability = Double

  type Prior[Label] = Label => Probability
  type Likelihood[Feature, Label] = Label => Feature => Probability

  type NaiveBayes[Feature,Label] = Iterable[Feature] => Dist[Label]
}

trait Domain {
  type Value <: Any
  def range: Option[DistData[Value]]
}

sealed trait Dist[A] {
  type Item = A
  type Probability = Double
  def domain: Domain

  def pdf(x: Item): Probability
}

case class DiscreteDist[A](
  m: Map[DiscreteDist[A]#Item, Dist[_]#Probability],
  domain: Domain
) extends Dist[A] {


  override def pdf(x: Item): Probability =
    if(m contains x)
      m(x)
    else
      0.0
}

object DiscreteDist {

  import DistData._

  type DiscreteMap[A] = Map[DiscreteDist[A]#Item, Dist[_]#Probability]

  def discreteDomain[A](m: DiscreteMap[A]): Domain =
    new Domain {
      type Value = A
      val range:Option[DistData[Value]] =
        Some(m.keys)
    }

  def apply[A](m: DiscreteMap[A]): DiscreteDist[A] =
    DiscreteDist(m, discreteDomain(m))

}

case class ContinuousDist[A](pdf:A => Double)