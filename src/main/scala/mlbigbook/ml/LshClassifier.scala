package mlbigbook.ml

import mlbigbook.wordcount.{ Data, DistData, Vector, Vectorizer }
import mlbigbook.lsh.LSH
import scala.util.Random
import mlbigbook.wordcount.Rank
import mlbigbook.wordcount.DistData


object LshClassifier {

  type Type = Data.Document => Labeled

  import Labeled._

  type LabedDocVector = (String, Vector)

  def initializeHashTables(bandSize:Int):Map[Int, Set[LabedDocVector]] = 
    (0 until bandSize).foldLeft(Map.empty[Int, Set[LabedDocVector]])(
      (m, band) => m + (band -> Set.empty[LabedDocVector]) 
    )

  def apply(
    nLSHFuncs:Int, 
    bandSize: Int)(
    dist: Vector.Distance, 
    kNeighborhoodSize:Int,
    mkVec: Vectorizer.Maker)(labeledCorpus: LabeledCorpus): Type = {

    val vectorizer = mkVec(labeledCorpus.corpus.map(_.example))
    val vectorizedLabeledData = labeledCorpus.corpus.map(d => (d.label, vectorizer(d.example)))

    val vectorspaceSize = vectorizedLabeledData.take(1).toSeq(0)._2.cardinality
    val lshFuncs = {
      implicit val rand = new Random()
      LSH.create(nLSHFuncs, vectorspaceSize, bandSize)
    }

    val hashTables = {
      val hts = vectorizedLabeledData.aggregate(initializeHashTables(bandSize))(
        (tables, labeledDocument) => 
          lshFuncs.map(h => h(labeledDocument._2)).foldLeft(tables)(
            (updatingTables, hashedIndex) => {
              val updatedSet = updatingTables(hashedIndex) + labeledDocument
              (updatingTables - hashedIndex) + (hashedIndex -> updatedSet)
            }
          ),
        (table1, table2) =>
          table1.foldLeft(table2)({
            case (updating, (hashedIndex, documentSet)) =>
               val updated4hashedIndex = (updating(hashedIndex) ++ documentSet)
               (updating - hashedIndex) + (hashedIndex -> updated4hashedIndex)
          })
      )

      (0 until bandSize).foldLeft(Seq.empty[Seq[LabedDocVector]])(
        (ht, index) => ht :+ hts(index).toSeq
      )
    }

    val perTableRankers = {
      
      hashTables.map(ht => 
        (vecInputDoc:Vector) =>
          ht
            .map({ case (doc, vec) => (dist(vec, vecInputDoc), doc) })
            .sortBy(-_._1)
            .take(kNeighborhoodSize)
      )
    }
      
    (inputDoc: Data.Document) => {
      val vecInputDoc = vectorizer(inputDoc)
      val hashIndicies = lshFuncs.map(h => h(vecInputDoc)).toSet
      val documentsFromAllBins = hashIndicies.flatMap(hIndex => perTableRankers(hIndex)(vecInputDoc))
      documentsFromAllBins.toList
        .sortBy(-_._1)
        .take(kNeighborhoodSize)
        .take(1)(0)._2
    }
  }
}
