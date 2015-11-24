package mlbigbook.ml

import java.util.Random

import mlbigbook.data._
import mlbigbook.data.mut.{ DenseVector, DenseVectorBuilder }

import scala.annotation.tailrec

/**
 * Input for the KMeans clustering algorithm.
 *
 * The Distance function (d) controls the k-means algorithm's notion of closeness
 * and farness. The nClusters value is the k in k-means; it's the number of clusters.
 *
 * The tolerance and maxIterations values describe the stopping conditions of the
 * k-means algorithm. If the absolute value of the sum of the differences in
 * cluster centers between two iterations is less than tolerance, then the algorithm
 * halts. Or, if the number of iterations exceeds maxIterations, then the algorithm
 * halts.
 */
case class KMeansIn(d: Distance, nClusters: Int, tolerance: Double, maxIterations: Int)

/**
 * Evaluated result from clustering. Describes indexed clusters.
 *
 * The Vectorizer[T] type (v) allows one to make vectors that will be in the
 * same space as these cluster centers. The cardinality is the size of the
 * vectorspace for all of the clusters. The centers value is this indexed
 * sequence of cluster centers.
 */
case class VectorizedCenters[T](cardinality: Int, v: Vectorizer[T], centers: IndexedSeq[Center])

/**
 * Represents a cluster center.
 *
 * The id value is helpful for naming clusters. The mean value is the vector
 * representation of the cluster's center.
 */
case class Center(id: String, mean: OldVector)

/** Contains methods for performing the k-means clustering algorithm. */
object KMeans {

  /** Creates randomized initial cluster centers and then runs the k-means algorithm. */
  def apply[T](k: KMeansIn)(vdata: VectorDataIn[T])(implicit rand: Random): VectorizedCenters[T] = {

    val (vectorizer, vectorized) = vdata()

    val cardinality = vectorized.take(1).toSeq.headOption.map(_._2.cardinality).getOrElse(0)

    val initialCenters = VectorizedCenters[T](
      cardinality,
      vectorizer,
      (0 until k.nClusters).map(id =>
        Center(id.toString, DenseVector.mkRandom(cardinality)))
    )

    apply(initialCenters)(k)(vectorized)
  }

  /**
   * Runs the k-means algorithm using the given centers.
   *
   * If the initial center's cardinality is non-positive, or if the KMeansIn's nClusters
   * doesn't equal the number of initial vectorized centers, then this method will
   * short-circuit and evaluate to the initial clusters. Otherwise, it will proceed
   * with k-means.
   */
  def apply[T](initial: VectorizedCenters[T])(k: KMeansIn)(vectorized: DataClass[(T, OldVector)]): VectorizedCenters[T] =
    if (initial.cardinality > 0 && k.nClusters == initial.centers.size)
      apply_h(k, initial, 0.0, 0, vectorized)
    else
      initial

  /** Performs the iterative assignment and update steps of the k-means algorithm. */
  @tailrec @inline private def apply_h[T](
    k:        KMeansIn,
    current:  VectorizedCenters[T],
    currTol:  Double,
    currIter: Int,
    data:     DataClass[(T, OldVector)]
  ): VectorizedCenters[T] =

    if (currIter >= k.maxIterations) {
      current

    } else {

      val updated = updateCenters(k, data, current)

      val updatedTol = calculateTolerance(current.centers, updated.centers)

      if (currIter > 0 && Math.abs(currTol - updatedTol) >= k.tolerance)
        updated
      else
        apply_h(k, updated, updatedTol, currIter + 1, data)
    }

  /** Performs a single assignment and update step of k-means. */
  def updateCenters[T](
    k:       KMeansIn,
    data:    DataClass[(T, OldVector)],
    current: VectorizedCenters[T]
  ): VectorizedCenters[T] = {

    // uses the current cluster centers to construct a classifier, which
    // will assign a datapoint to the nearest cluster
    val clusterAssigner = HardCluster(k.d)(current)

    val (newCenterBuilders, nVecs) =
      // assign each vector in the data to a cluster center using the current centers
      assignment(clusterAssigner)(data)
        // create empty dense vector builders
        // and
        // aggregate each new center by summing all vectors that are assigned to a center
        // also keep track of the number of vectors we see
        .aggregate((mkCenterBuilders(current), 0.0))(
          {
            case ((cbs, n), (assignedCenter, vector)) =>
              // mutates the DenseVectorBuilder that's in the mapping
              cbs(assignedCenter.label)._1.add(vector)
              // keep the same mapping, increase the # of observed instances
              (cbs, n + 1)
          },
          {
            case ((cbs1, n1), (cbs2, n2)) =>

              val updatedCB =
                cbs1.keys.foldLeft(cbs2)({
                  case (updatingCbs2, id) =>
                    // mutates dense vector builder in mapping
                    updatingCbs2(id)._1.add(cbs1(id)._1)
                    // keep mapping the same
                    updatingCbs2
                })

              (updatedCB, n1 + n2)
          }
        )

    val newCenters =
      newCenterBuilders.toIndexedSeq
        // get the centers back in their original order
        .sortBy(_._2._2)
        .map({
          case (id, centerBuilder) =>
            // mutates the dense vector builder
            // divide the summed vector by tvecorizedshe # of observed vectors to obtain
            // the mean: the new, updated, center
            centerBuilder._1.normalize(nVecs)
            // construct a side-effect free vector from this builder
            Center(id, centerBuilder._1.create(copyValues = false))
        })

    current.copy[T](centers = newCenters)
  }

  /**
   * The assignment step.
   *
   * Uses the Classifier to assign a label to each datapoint. When this Classifier is
   * a HardCluster, this is equivalent to assigning each datapoint to the nearest cluster.
   */
  def assignment[T](c: Learning[T, Labeled]#Classifier)(data: DataClass[(T, OldVector)]): DataClass[(Labeled, OldVector)] =
    data.map({ case (item, vector) => (c(item), vector) })

  /**
   * Create mutable vector-builders, one for each cluster center.
   * Used in the update step. Mutability is required for efficiency.
   */
  def mkCenterBuilders(prevCenters: VectorizedCenters[_]): Map[String, (DenseVectorBuilder, Int)] =
    prevCenters.centers.zipWithIndex
      .map({
        case (c, index) =>
          (c.id, (new DenseVectorBuilder(prevCenters.cardinality), index))
      }).toMap

  /**
   * Evaluates the primary stopping condition of the k-means algorithm.
   *
   * Computes the sum of the absolute value of the difference between each pair of
   * cluster centers: one from prev and one from curr.
   */
  def calculateTolerance(prev: IndexedSeq[Center], curr: IndexedSeq[Center]): Double =
    prev.zip(curr)
      .foldLeft(0.0) {
        case (tol, (prevCenter, currCenter)) =>
          tol + OldVector.absElemDiff(prevCenter.mean, currCenter.mean)
      }

}