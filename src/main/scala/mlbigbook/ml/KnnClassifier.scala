/*
 * A k-Nearest Neighbor classifier implementation.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag

object KnnClassifier {

  import Labeled.str2labeled
  import Vectorizer.fn2vectorizer
  import VectorizerMaker._
  import Classifier._

  /**
   * Creates a k-Nearest Neighbors classifier.
   *
   * Uses NnRanker.apply underneath to perform the nearest neighbors search.
   */
  def apply[T: ClassTag](
    dist: Distance,
    kNeighborhoodSize: Int,
    mkVec: VectorizerMaker[T],
    labeledCorpus: DistData[LabeledData[T]]): Classifier[T] = {

    val unlabeledVectorizerMaker = (d: DistData[LabeledData[T]]) => {
      val vectorizerT = mkVec(d.map(_.example))
      fn2vectorizer(
        (labeled: LabeledData[T]) => vectorizerT(labeled.example)
      )
    }

    val nn = NnRanker(dist, kNeighborhoodSize, unlabeledVectorizerMaker, labeledCorpus)

    (input: T) => {
      val neighborhood = nn(LabeledData.unlabeled(input)).map(_._2.label)
      str2labeled(
        takeLargest(countVotes(neighborhood))
      )
    }
  }

  /**
   * Counts the number of times each element occurs in neighborhood.
   * Returns this information as a mapping.
   */
  def countVotes(neighborhood: Traversable[String]): Map[String, Int] =
    neighborhood.foldLeft(Map.empty[String, Int])(
      (m, label) =>
        if (m.contains(label)) {
          val newCount = m(label) + 1
          (m - label) + (label -> newCount)
        } else {
          m + (label -> 1)
        }
    )

  /**
   * Evaluates to the String associated with the largest value (of Numeric type N). If the input
   * elements is empty, evaluates to the empty string ("").
   */
  @inline def takeLargest[N](elements: Map[String, N])(implicit n: Numeric[N]): String =
    takeLargest(elements.toIndexedSeq)

  /**
   * Evaluates to the String associated with the largest value (of Numeric type N). If the input
   * elements is empty, evaluates to the empty string ("").
   */
  def takeLargest[N](elements: IndexedSeq[(String, N)])(implicit n: Numeric[N]): String =
    elements.size match {

      case 0 =>
        ""

      case 1 =>
        elements.head._1

      case _ =>
        elements.slice(1, elements.size)
          .foldLeft(elements.head)({
            case ((maxLabel, maxValue), (label, value)) =>
              if (n.gt(value, maxValue))
                (label, value)
              else
                (maxLabel, maxValue)
          })._1

    }

}