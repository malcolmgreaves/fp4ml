package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag
import scala.util.Random

object LshRanker {

  import Ranker._

  def apply[T](
    nLSHFuncs: Int,
    bandSize: Int)(
      dist: Distance,
      kNeighborhoodSize: Int,
      mkVec: VectorizerMaker[T],
      data: DistData[T])(
        implicit ddContext: DistDataContext): Ranker[T] = {

    val vectorizer = mkVec(data)
    val vectorizedData = data.map(d => (d, vectorizer(d)))

    val lshFuncs = {
      val vectorspaceSize = vectorizedData.take(1).toSeq.head._2.cardinality
      implicit val rand = new Random()
      LSH(nLSHFuncs, vectorspaceSize, bandSize)
    }

    val hashTables = createHashTablesForCorpus(bandSize, lshFuncs, vectorizedData)

    val perTableRankers = hashTables.map(ht =>
      (vecInput: Vector) =>
        Ranker.takeTopK[T, Double](
          kNeighborhoodSize,
          ht.map({
            case (d, vec) => (dist(vec, vecInput), d)
          })
        )
    ).toIndexedSeq

    (input: T) => {

      val vecInput = vectorizer(input)

      val hashIndicies = lshFuncs.map(lsh => lsh(vecInput)).toSet

      val examplesFromAllBins = hashIndicies.flatMap(hIndex => perTableRankers(hIndex)(vecInput))

      Ranker.takeTopK(
        kNeighborhoodSize,
        examplesFromAllBins
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
