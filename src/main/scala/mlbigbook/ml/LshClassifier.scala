package mlbigbook.ml

//import mlbigbook.data
//import mlbigbook.data.{VectorizerMaker, DistDataContext, DistData, Data}
//import mlbigbook.wordcount._
//import mlbigbook.lsh.LSH
//import scala.util.Random
//
//object LshClassifier {
//
//  type Type = Data.Document => Labeled
//
//  import Labeled._
//
//  type LabeledDocVector = (String, data.Vector)
//
//  def createHashTablesForCorpus(
//    bandSize: Int,
//    lshFuncs: Seq[LSH.Type],
//    vectorizedLabeledData: DistData[(String, data.Vector)])(
//      implicit ddContext: DistDataContext): Seq[DistData[LabeledDocVector]] = {
//
//    vectorizedLabeledData
//      .map({
//        case (label, vector) => ((label, vector), lshFuncs.map(h => h(vector)).toSet)
//      })
//      .flatMap({
//        case (labeledVector, hashIndices) => hashIndices.map(hIndex => (hIndex, labeledVector))
//      })
//      .groupBy(_._1)
//      .map({
//        case (hIndex, iterable) => (hIndex, iterable.map(_._2))
//      })
//      .toSeq
//      .map({
//        case (hIndex, labeledVectorItr) => (hIndex, ddContext.from(labeledVectorItr))
//      })
//      .sortBy(_._1)
//      .map(_._2)
//  }
//
//  def apply(
//    nLSHFuncs: Int,
//    bandSize: Int)(
//      dist: Distance,
//      kNeighborhoodSize: Int)(
//        mkVec: VectorizerMaker[Data.Document],
//        labeledCorpus: LabeledCorpus)(implicit ddContext: DistDataContext): Type = {
//
//    val vectorizer = mkVec(labeledCorpus.corpus.map(_.example))
//    val vectorizedLabeledData = labeledCorpus.corpus.map(d => (d.label, vectorizer(d.example)))
//
//    val lshFuncs = {
//      val vectorspaceSize = vectorizedLabeledData.take(1).toSeq(0)._2.cardinality
//      implicit val rand = new Random()
//      LSH.create(nLSHFuncs, vectorspaceSize, bandSize)
//    }
//
//    val hashTables = createHashTablesForCorpus(bandSize, lshFuncs, vectorizedLabeledData)
//
//    val perTableRankers = hashTables.map(ht =>
//      (vecInputDoc: data.Vector) =>
//        Ranker.takeTopK(
//          kNeighborhoodSize,
//          ht.map({ case (doc, vec) => (dist(vec, vecInputDoc), doc) })
//        )
//    )
//
//    (inputDoc: Data.Document) => {
//
//      val vecInputDoc = vectorizer(inputDoc)
//
//      val hashIndicies = lshFuncs.foldLeft(Set.empty[Int])(
//        (hIndexSet, h) => hIndexSet + h(vecInputDoc)
//      )
//
//      val documentsFromAllBins = hashIndicies.flatMap(hIndex => perTableRankers(hIndex)(vecInputDoc))
//
//      val topDocumentsFromBins = Rank.takeTopK(
//        kNeighborhoodSize,
//        documentsFromAllBins
//      )
//
//      NearestNeighbors.takeLargest(
//        NearestNeighbors.countNeighborhoodVotes(topDocumentsFromBins.map(_._2)).toIndexedSeq
//      )
//    }
//  }
//}
