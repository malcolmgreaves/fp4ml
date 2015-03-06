package mlbigbook.ml

import mlbigbook.wordcount.{ Data, DistData, Vector, Vectorizer, Rank }
import mlbigbook.lsh.LSH
import scala.util.Random
import mlbigbook.wordcount.Rank
import mlbigbook.wordcount.DistData

object LshClassifier {

  type Type = Data.Document => Labeled

  import Labeled._

  type LabeledDocVector = (String, Vector)

  def initializeHashTables(bandSize: Int): Map[Int, Set[LabeledDocVector]] =
    (0 until bandSize).foldLeft(Map.empty[Int, Set[LabeledDocVector]])(
      (m, band) => m + (band -> Set.empty[LabeledDocVector])
    )

  def createHashTablesForCorpus(
    bandSize:Int, 
    lshFuncs:Seq[LSH.Type],
    vectorizedLabeledData:DistData[(String, Vector)]):Seq[Seq[LabeledDocVector]] = {

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

      (0 until bandSize).foldLeft(Seq.empty[Seq[LabeledDocVector]])(
        (ht, index) => ht :+ hts(index).toSeq
      )
  }

  def apply(
    nLSHFuncs: Int,
    bandSize: Int)(
    dist: Vector.Distance,
    kNeighborhoodSize: Int)(
    mkVec: Vectorizer.Maker,
    labeledCorpus: LabeledCorpus): Type = {

    val vectorizer = mkVec(labeledCorpus.corpus.map(_.example))
    val vectorizedLabeledData = labeledCorpus.corpus.map(d => (d.label, vectorizer(d.example)))

    val lshFuncs = {
      val vectorspaceSize = vectorizedLabeledData.take(1).toSeq(0)._2.cardinality
      implicit val rand = new Random()
      LSH.create(nLSHFuncs, vectorspaceSize, bandSize)
    }

    val hashTables = createHashTablesForCorpus(bandSize, lshFuncs, vectorizedLabeledData)

    val perTableRankers = hashTables.map(ht =>
      (vecInputDoc: Vector) =>
        Rank.takeTopK(
          kNeighborhoodSize,
          ht.map({ case (doc, vec) => (dist(vec, vecInputDoc), doc) })
        )
    )

    (inputDoc: Data.Document) => {

      val vecInputDoc = vectorizer(inputDoc)

      val hashIndicies = lshFuncs.foldLeft(Set.empty[Int])(
        (hIndexSet, h) => hIndexSet + h(vecInputDoc)
      )

      val documentsFromAllBins = hashIndicies.flatMap(hIndex => perTableRankers(hIndex)(vecInputDoc))

      val topDocumentsFromBins = Rank.takeTopK(
        kNeighborhoodSize,
        documentsFromAllBins
      )

      NearestNeighbors.takeLargest(
        NearestNeighbors.countNeighborhoodVotes(topDocumentsFromBins.map(_._2)).toIndexedSeq
      )
    }
  }
}
