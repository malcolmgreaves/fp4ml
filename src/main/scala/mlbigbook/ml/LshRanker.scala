package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag
import scala.util.Random

object LshRanker {

  def apply[T](nLshFuncs: Int, nBins: Int)(n: NearNeighIn)(vdata: VectorDataIn[T])(
    implicit ddContext: DistDataContext, rand: Random): Ranker[T] = {

    val (vectorizer, vectorizedData) = vdata()

    val lshFuncs = Lsh(nLshFuncs)(
      LshIn(
        vectorizedData.take(1).toSeq.head._2.cardinality,
        nBins
      )
    )

    val hashTables = createHashTables(nBins, lshFuncs, vectorizedData)

    val perTableRankers = hashTables.map(ht =>
      (vecInput: Vector) =>
        Ranker.takeTopK[T, Double](
          n.neighborhoodSize,
          ht.map({
            case (item, vecItem) => (item, n.dist(vecInput, vecItem))
          })
        )
    ).toIndexedSeq

    (input: T) => {

      val vecInput = vectorizer(input)

      val hashIndicies = lshFuncs.map(lsh => lsh(vecInput)).toSet

      val examplesFromAllBins = hashIndicies.flatMap(hIndex => perTableRankers(hIndex)(vecInput))

      Ranker.takeTopK(
        n.neighborhoodSize,
        examplesFromAllBins
      )
    }
  }

  def createHashTables[T](
    bandSize: Int,
    lshFuncs: Seq[Lsh],
    vectorizedData: DistData[(T, Vector)])(
      implicit ddContext: DistDataContext): Seq[DistData[(T, Vector)]] =

    vectorizedData
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
