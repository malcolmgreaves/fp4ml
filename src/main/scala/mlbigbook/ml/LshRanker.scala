package mlbigbook.ml

import mlbigbook.data._

import scala.reflect.ClassTag
import scala.util.Random

object LshRanker {

  def apply[T](nLshFuncs: Int, nBins: Int)(n: NearNeighIn)(vdata: VectorDataIn[T])(
    implicit
    ddContext: DataContext, rand: Random
  ): Ranker[T] = {

    val (vectorizer, vectorizedData) = vdata()

    val lshFuncs = Lsh(nLshFuncs)(
      LshIn(
        vectorizedData.take(1).toSeq.head._2.cardinality,
        nBins
      )
    )

    val hashTables = createHashTables(lshFuncs, vectorizedData)

    val perTableRankers = hashTables.map(ht =>
      (vecInput: OldVector) =>
        Ranker.takeTopK[T](
          (v: OldVector) => n.dist(vecInput, v),
          n.neighborhoodSize,
          ht
        )).toIndexedSeq

    (input: T) => {

      val vecInput = vectorizer(input)

      val hashIndicies = lshFuncs.map(lsh => lsh(vecInput)).toSet

      val examplesFromAllBins = hashIndicies.flatMap(hIndex => perTableRankers(hIndex)(vecInput))

      val f = (v: OldVector) => n.dist(vecInput, v)

      Ranker.takeTopK[T](
        f,
        n.neighborhoodSize,
        examplesFromAllBins
      ).map(x => (x._1, f(x._2)))
    }
  }

  def createHashTables[T](lshFuncs: Seq[Lsh], vdata: DataClass[(T, OldVector)])(implicit ddContext: DataContext): Seq[DataClass[(T, OldVector)]] =

    vdata
      // use the LSH functions to compute the set of hash table indicies
      // for each (T, Vector) pair
      // and output each individual index with this pair
      .flatMap({
        case dataAndVector @ (_, v) =>
          val hashIndicies = lshFuncs.map(h => h(v)).toSet
          hashIndicies.map(hIndex => (hIndex, dataAndVector))
      })
      // group the data + vector pairs by their hash indicies
      .groupBy(_._1)
      // strip the index from the iterable of each data-vector triple,
      // so that we only have pairs of hash index and an iterable of
      // associated data-value pairs
      .map({
        case (hIndex, iterable) =>
          (hIndex, iterable.map(_._2))
      })
      // convert this Data[...] into a Seq[...], (size lshFuncs.size)
      .toSeq
      // convert each Iterable inside each Seq element into a Data instance
      // using the DataContext implicit value
      .map({
        case (hIndex, dataAndVectorItr) =>
          (hIndex, ddContext.from(dataAndVectorItr))
      })
      // re-arrange this Seq into ascending order on the hash index
      .sortBy(_._1)
      // finally, remove these (now redundant) indicies
      .map(_._2)

}
