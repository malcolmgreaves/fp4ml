package mlbigbook.ml

import mlbigbook.wordcount._
import mlbigbook.lsh.LSH
import scala.util.Random
import mlbigbook.ml.LabeledCorpus

object LshClassifier {

  type Type = Data.Document => Labeled

  import Labeled._

  type LabeledDocVector = (String, Vector)

  def createHashTablesForCorpus(
    bandSize: Int,
    lshFuncs: Seq[LSH.Type],
    vectorizedLabeledData: DistData[(String, Vector)])(implicit ddContx:DistDataContext): Seq[DistData[LabeledDocVector]] = {

    val hts: DistData[(Int, Iterable[LabeledDocVector])] =
      vectorizedLabeledData
        .map({
          case (label, vector) => ((label, vector), lshFuncs.map(h => h(vector)).toSet)
        }).flatMap({
          case (labeledVector, hashIndices) => hashIndices.map(hIndex => (hIndex, labeledVector))
        }).groupBy(_._1)
        .map({
          case (hIndex, iterable) => (hIndex, iterable.map(_._2))
        })

    val hashTables:Seq[DistData[LabeledDocVector]] = {

      ???
    }

    hashTables
  }

  def apply(
    nLSHFuncs: Int,
    bandSize: Int)(
      dist: Vector.Similarity,
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
