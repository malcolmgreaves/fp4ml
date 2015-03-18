package mlbigbook.ml

import mlbigbook.data._
import scala.reflect.ClassTag
import scala.util.Random

object LshClassifier {

  import Classifier._
  import Labeled.str2labeled

  def apply[T: ClassTag](
    nLSHFuncs: Int,
    bandSize: Int)(
      dist: Distance,
      kNeighborhoodSize: Int,
      mkVec: VectorizerMaker[T],
      labeledCorpus: DistData[LabeledData[T]])(
        implicit ddContext: DistDataContext): Classifier[T] = {

    val vectorizer = mkVec(labeledCorpus.map(_.example))
    val vectorizedLabeledData = labeledCorpus.map(d => (d, vectorizer(d.example)))

    val lshFuncs = {
      val vectorspaceSize = vectorizedLabeledData.take(1).toSeq.head._2.cardinality
      implicit val rand = new Random()
      LSH(nLSHFuncs, vectorspaceSize, bandSize)
    }

    val hashTables = createHashTablesForCorpus(bandSize, lshFuncs, vectorizedLabeledData)

    val perTableRankers = hashTables.map(ht =>
      (vecInput: Vector) =>
        Ranker.takeTopK[LabeledData[T], Double](
          kNeighborhoodSize,
          ht.map({
            case (labeledData, vec) => (dist(vec, vecInput), labeledData)
          })
        )
    ).toIndexedSeq

    (input: T) => {

      val vecInput = vectorizer(input)

      val hashIndicies = lshFuncs.map(lsh => lsh(vecInput)).toSet

      val examplesFromAllBins = hashIndicies.flatMap(hIndex => perTableRankers(hIndex)(vecInput))

      val topLabelsFromAllBins = Ranker.takeTopK(
        kNeighborhoodSize,
        examplesFromAllBins
      ).map(_._2.label)

      str2labeled(
        KnnClassifier.takeLargest(
          KnnClassifier.countVotes(topLabelsFromAllBins)
        )
      )
    }
  }

  def createHashTablesForCorpus[T](
    bandSize: Int,
    lshFuncs: Seq[LSH],
    vectorizedLabeledData: DistData[(T, Vector)])(
      implicit ddContext: DistDataContext): Seq[DistData[(T, Vector)]] =

    vectorizedLabeledData
      .map({
        case (data, vector) =>
          ((data, vector), lshFuncs.map(h => h(vector)).toSet)
      })
      .flatMap({
        case (dataAndVector, hashIndices) =>
          hashIndices.map(hIndex => (hIndex, dataAndVector))
      })
      .groupBy(_._1)
      .map({
        case (hIndex, iterable) =>
          (hIndex, iterable.map(_._2))
      })
      .toSeq
      .map({
        case (hIndex, dataAndVectorItr) =>
          (hIndex, ddContext.from(dataAndVectorItr))
      })
      .sortBy(_._1)
      .map(_._2)
}
