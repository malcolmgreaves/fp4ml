package mlbigbook.util

import mlbigbook.data.{ VectorizedData, DataClass }
import org.apache.log4j.Logger

import scala.util.Random

/**
 * Sampling helper code. Includes an implementation of sampling without replacement for Scala collections,
 * mini-batch sampling (both at the level of Data (pre-made VectorizedData matrices)
 * and within the matrices (in case the count of elements of Data is less than 1/miniBatchFraction.
 *
 * @author Marek Kolodziej
 */
object Sampling {

  @transient val log = Logger.getLogger(Sampling.getClass)

  /**
   * Helper for sampling with and without replacement of Scala collections.
   *
   * @param coll
   * @param sampleSize
   * @param withReplacement
   * @param seed
   * @tparam A
   * @return
   */
  def sample[A](coll: Traversable[A], sampleSize: Int,
                withReplacement: Boolean, seed: Long = System.nanoTime): IndexedSeq[A] = {

    val indexed = coll.toIndexedSeq

    val rand = new Random(seed)

      /* Tail-recursive helper for sampling without replacement.
       Add picked element to acc and remove it from seq so
       it can't be chosen again.
     */
      @annotation.tailrec
      def collect(seq: IndexedSeq[A], size: Int, acc: List[A]): List[A] = {
        if (size == 0) acc
        else {
          val index = rand.nextInt(seq.size)
          collect(seq.updated(index, seq(0)).tail, size - 1, seq(index) :: acc)
        }
      }

      // simple sampling with replacement
      def withRep: IndexedSeq[A] =
        for (i <- 1 to sampleSize)
          yield indexed(rand.nextInt(coll.size))

    if (withReplacement)
      withRep
    else
      collect(indexed, sampleSize, Nil).toIndexedSeq
  }

  /**
   * Helper to sample for mini-batches. If the numer of items in the Data instance
   * (Scala collection or RDD) is greater than or equal to 1/miniBatchFraction, use
   * Data's own sampling (note that we're using exactSampleWithoutReplacment() as
   * opposed to sample, because Spark has a bug in its sample() method. When it is fixed,
   * this will default back to calling sample(). When the number of items in the
   * Data instance is less than 1/miniBatchFraction, we need to sample directly from the
   * VectorizedData instances (rows within the matrices).
   *
   * Example: if we have 100 elements in Data (100 instances of VectorizedData),
   * then sampling 0.01 (from 1/0.01 = 100 elements) means choosing 1 element at random.
   * However, if we have 10 VectorizedData instances in a Data instance, then sampling
   * 1% is impossible without sampling 1% directly from each VectorizedData element
   * within Data. The latter is slower, but compensates for too much aggregation
   * of individual examples into VectorizedData instances.
   *
   * @param data
   * @param miniBatchFraction
   * @param currSeed
   * @return
   */
  def sampleMiniBatch(
    data:              DataClass[VectorizedData],
    miniBatchFraction: Double,
    currSeed:          Long
  ): DataClass[VectorizedData] = {

    val collCount = data.size

    val regularSampling = collCount >= math.ceil(1.0 / miniBatchFraction)

    if (regularSampling) {

      data.exactSample(fraction = miniBatchFraction, seed = currSeed)

    } else {

      data.map {

        case v: VectorizedData =>

          val size = v.target.activeSize
          val rounded = math.max(1, math.round(miniBatchFraction * size).toInt)

          val rowIndices = sample(
            coll = (0 until size),
            sampleSize = rounded,
            withReplacement = false,
            seed = currSeed
          )

          VectorizedData(
            target = v.target(rowIndices).toDenseVector,
            features = v.features(rowIndices, ::).toDenseMatrix
          )
      }
    }
  }
}