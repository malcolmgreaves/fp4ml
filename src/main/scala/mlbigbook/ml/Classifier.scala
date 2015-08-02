package mlbigbook.ml

import mlbigbook.data._

/** Type representing something that can assign classifications to a type of object. */
trait Classifier[T] extends (T => Labeled)

object Classifier {

  implicit class Fn[T](val f: T => Labeled) extends Classifier[T] {
    override def apply(x: T) = f(x)
  }

}

/*

/**
 * Module specifying types for machine learning algorithms.
 *
 * @tparam A The instance type.
 * @tparam B The label type.
 */
trait Learning[A, B] {

  type Instance = A

  type Label = B

  type Pair = (Instance, Label)

  type TrainingData = Dat[Pair]

  type Classifier = Instance => Label

  type Estimator = Instance => DiscreteDistribution[Label]

  type Learner = TrainingData => (Classifier, Estimator)
}



 */ 