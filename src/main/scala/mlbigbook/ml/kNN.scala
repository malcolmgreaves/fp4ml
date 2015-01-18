package mlbigbook.ml

import mlbigbook.wordcount.{Data, DistData, Vector, Vectorizer}

trait Labeled {
  def label:String
}

object Labeled {
  implicit def str2labeled(s:String):Labeled =
    new Labeled {
      val label = s
    }
}

case class LabeledData[T](label:String, example:T) extends Labeled

case class LabeledCorpus(corpus:DistData[LabeledData[Data.Document]])

object KNN {

  type Type = Data.Document => Labeled

  import Labeled._

  def apply(
    dist: Vector.Distance,
    kNeighborhoodSize: Int,
    mkVec: Vectorizer.Maker)(labeledCorpus:LabeledCorpus): Type = {

    val vectorizer = mkVec(labeledCorpus.corpus.map(_.example))
    val vectorizedLabeledDocuments = labeledCorpus.corpus.map(d => (d.label, vectorizer(d.example)))

    (inputDoc: Data.Document) => {

      val vecInputDoc = vectorizer(inputDoc)
      
      val neighborhood = vectorizedLabeledDocuments
        .map({ case (label, vec) => (dist(vec, vecInputDoc), label) })
        .sortBy(-_._1)
        .take(kNeighborhoodSize)

      val neighCounts = neighborhood.foldLeft(Map.empty[String,Int])({
        case (m, (neighbor, label)) => 
          if(m.contains(label)) {
            val newCount = m(label) + 1
            (m - label) + (label -> newCount)
          } else {
           m + (label -> 1)
          }
      })

      neighCounts.toList
        .sortBy(-_._2)
        .take(1)(0)._1

    }
  }
}
