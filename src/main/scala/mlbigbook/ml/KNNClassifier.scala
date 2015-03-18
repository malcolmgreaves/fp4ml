package mlbigbook.ml

import mlbigbook.data._

object KNNClassifier {

  import Labeled.str2labeled
  import Classifier._

  def apply[T](
    dist: Distance,
    kNeighborhoodSize: Int,
    mkVec: VectorizerMaker[T],
    labeledCorpus: DistData[LabeledData[T]]): Classifier[T] = {

    val vectorizer = mkVec(labeledCorpus.map(_.example))
    val vectorizedLabeledDocuments = labeledCorpus.map(d => (d.label, vectorizer(d.example)))

    (input: T) => {

      val vecInputDoc = vectorizer(input)

      val neighborhood = Ranker.takeTopK(
        kNeighborhoodSize,
        vectorizedLabeledDocuments.map({ case (label, vec) => (dist(vec, vecInputDoc), label) })
      )

      str2labeled(
        NearestNeighbors.takeLargest(
          NearestNeighbors.countNeighborhoodVotes(neighborhood.map(_._2)).toIndexedSeq
        )
      )
    }
  }

}

object NearestNeighbors {

  /**
   * Counts the number of times each element occurs in neighborhood.
   * Returns this information as a mapping.
   */
  def countNeighborhoodVotes(neighborhood: Traversable[String]): Map[String, Int] =
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
  def takeLargest[N](elements: IndexedSeq[(String, N)])(implicit n: Numeric[N]): String =
    elements.size match {

      case 0 =>
        ""

      case 1 =>
        elements(0)._1

      case _ =>
        elements.toIndexedSeq.slice(1, elements.size)
          .foldLeft(elements(0))({
            case ((maxLabel, maxValue), (label, value)) =>
              if (n.gt(value, maxValue))
                (label, value)
              else
                (maxLabel, maxValue)
          })._1

    }

}