package mlbigbook.ml

import mlbigbook.data._

case class NaiveBayesIn(yes: String, no: String)

object NaiveBayes {

}

object NaiveBayesMulticlass {

  def apply[T](knownClasses: Seq[Labeled])(vdata: VectorDataIn[LabeledData[T]]): Seq[ProbabilityEstimater[T]] =
    ???
  
}

object NaiveBayesBinary {

  def apply[T](yes: Labeled, no: Labeled)(vdata: VectorDataIn[LabeledData[T]]): ProbabilityEstimater[T] =
    ???

}

trait ProbabilityEstimater[T] extends (T => Double)

object ProbabilityClassifier {

  import Classifier._

  def apply[T](yes: Labeled, no: Labeled, threshold: Double)(p: ProbabilityEstimater[T]): Classifier[T] =
    (input: T) =>
      if (p(input) >= threshold)
        yes
      else
        no

  def apply[T](knownClasses: Seq[Labeled])(ps: Seq[ProbabilityEstimater[T]]): Classifier[T] =
    (input: T) => {

      val estimatesAndClasses = ps.map(p => p(input)).zip(knownClasses)

      estimatesAndClasses.headOption match {

        case Some((estimate, labeled)) =>
          estimatesAndClasses.slice(1, estimatesAndClasses.length)
            .foldLeft((estimate, labeled))({

              case ((maxEstimate, maxLabeled), (nextEstimate, nextLabeled)) =>
                if (nextEstimate > maxEstimate)
                  (nextEstimate, nextLabeled)
                else
                  (maxEstimate, maxLabeled)
            })._2

        case None =>
          UnlabeledData.asLabled
      }
    }

}
