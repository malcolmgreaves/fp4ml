package mlbigbook.ml

import mlbigbook.wordcount.{ Data, DistData, Vector, Vectorizer, Rank }

trait Labeled {
  def label: String
}

object Labeled {
  implicit def str2labeled(s: String): Labeled =
    new Labeled {
      val label = s
    }
}

case class LabeledData[T](label: String, example: T) extends Labeled

case class LabeledCorpus(corpus: DistData[LabeledData[Data.Document]])

object KNN {

  type Type = Data.Document => Labeled

  import Labeled._

  def apply(
    dist: Vector.Distance,
    kNeighborhoodSize: Int)(
    mkVec: Vectorizer.Maker,
    labeledCorpus: LabeledCorpus): Type = {

    val vectorizer = mkVec(labeledCorpus.corpus.map(_.example))
    val vectorizedLabeledDocuments = labeledCorpus.corpus.map(d => (d.label, vectorizer(d.example)))

    (inputDoc: Data.Document) => {

      val vecInputDoc = vectorizer(inputDoc)

      val neighborhood = Rank.takeTopK(
        kNeighborhoodSize,
        vectorizedLabeledDocuments.map({ case (label, vec) => (dist(vec, vecInputDoc), label) })
      )

      NearestNeighbors.takeLargest(
        NearestNeighbors.countNeighborhoodVotes(neighborhood.map(_._2)).toIndexedSeq
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