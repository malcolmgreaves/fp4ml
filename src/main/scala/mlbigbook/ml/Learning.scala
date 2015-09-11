package mlbigbook.ml

import mlbigbook.data.Data

import scala.language.postfixOps
import scala.reflect.ClassTag

/**
 * Module specifying types for machine learning algorithms.
 *
 * @tparam A The instance type.
 * @tparam B The label type.
 */
trait Learning[A, B] {

  type Instance = A
  type Label = B

  type Classifier = Instance => Label
  type Estimator = Instance => DiscreteDistribution[Label]

  type Pair = (Instance, Label)
  type TrainingData = Data[Pair]

  type Learner = TrainingData => (Classifier, Estimator)
}

case class DiscreteEstimator[F: ClassTag, Label](estimate: Learning[Feature.Vector[F], Label]#Estimator) {

  implicit val ev: Val[(Label, Distribution.Probability)] =
    TupleVal2[Label]

  val classify: Learning[Feature.Vector[F], Label]#Classifier =
    (x: Feature.Vector[F]) => Argmax(estimate(x) toSeq)._1
}