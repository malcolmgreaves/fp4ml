package mlbigbook.math

import breeze.linalg.Vector
import fif.Data
import Data.ops._
import mlbigbook.ml.Stats

import scala.language.higherKinds

/**
 * An online, streaming algorithm for calculating mean and variance.
 *
 * @author Marek Kolodziej
 * @author Malcolm Greaves
 */
object OnlineMeanVariance {

  /**
   * A snapshot of the variables necessary to calculate mean and variance in
   * Welford's online algorithm. The `toStats` method uses these variables to
   * perform the final variance calculation.
   */
  case class State[N: NumericConversion, V[_] <: Vector[_]](count: Long, mean: V[N], m2: V[N]) {

    def toStats()(implicit ops: VectorOpsT[N, V]): Stats[V[N]] =
      Stats(
        count = count,
        mean = mean,
        variance =
        if (count < 2l)
          ops.zeros(mean.size)
        else
          ops.divS(
            m2,
            implicitly[NumericConversion[N]].fromLong(count - 1l)
          )
      )
  }

  /**
   * Adds a vector to the existing state, producing updated state.
   */
  def update[N: NumericConversion, V[_] <: Vector[_]](
    existing: State[N, V],
    current:  V[N]
  )(implicit ops: VectorOpsT[N, V]): State[N, V] = {

    val nc = implicitly[NumericConversion[N]]

    val newN = existing.count + 1l
    // delta := current - mean
    val delta = ops.subV(current, existing.mean)
    // newMean := mean + (delta / newN)
    val newMean = ops.addV(existing.mean, ops.divS(delta, nc.fromLong(newN)))
    // newM2 := m2 + (delta * (current - newMean))
    val newM2 = ops.addV(existing.m2, ops.mulV(delta, ops.subV(current, newMean)))

    State[N, V](count = newN, mean = newMean, m2 = newM2)
  }

  /**
   * Uses Welford's method to calculate variance of the input Data. Starts
   * using the input existing state.
   *
   * The resulting State contains all information necessary to recover
   * the number of counted data points, their average, and their variance.
   * (See `State.toStats` for conversion methods).
   *
   * Note that since this method assumes an existing state, the returned State's
   * values will be appropriate for all of the prior data that went into creating
   * the existing state as well as this additional data.
   */
  def apply[D[_]: Data, N: NumericConversion, V[_] <: Vector[_]](
    existing: State[N, V],
    elems:    D[V[N]]
  )(implicit ops: VectorOpsT[N, V]): State[N, V] =
    elems.headOption match {

      case Some(v) =>
        assert(v.size == existing.mean.size)
        val nc = implicitly[NumericConversion[N]]
        elems
          .aggregate(existing)(
            update[N, V],
            {
              case (State(n1, mean1, m21), State(n2, mean2, m22)) =>
                val newN = n1 + n2
                // delta := mean2 - mean1
                val delta = ops.subV(mean2, mean1)
                // newMean := ...
                val newMean =
                  if (n2 * 10l < n1)
                    //                      mean1 + (delta * n2) / (n1 + n2)
                    ops.addV(mean1, ops.divS(ops.mulS(delta, nc.fromLong(n2)), nc.fromLong(newN)))
                  else if (n1 * 10 < n2)
                    //                      mean2 - (delta * n1) / (n1 + n2)
                    ops.subV(mean2, ops.divS(ops.mulS(delta, nc.fromLong(n1)), nc.fromLong(newN)))
                  else
                    //                      (mean1 * n1 + mean2 * n2) / (n1 + n2)
                    ops.divS(ops.addV(ops.mulS(mean1, nc.fromLong(n1)), ops.mulS(mean2, nc.fromLong(n2))), nc.fromLong(newN))

                //                  val newM = m21 + m22 + (math.pow(delta, 2) * n1 * n2) / (n1 + n2)
                val newM = {
                  val deltaSquared = ops.mulV(delta, delta)
                  // fraction := ((delta * delta) * n1 * n2) / (n1 + n2)
                  val fraction =
                    ops.divS(
                      ops.mulS(ops.mulS(deltaSquared, nc.fromLong(n1)), nc.fromLong(n2)),
                      nc.fromLong(newN)
                    )

                  // := m21 + m22 + ((delta * delta) * n1 * n2) / (n1 + n2)
                  ops.addV(
                    ops.addV(m21, m22), fraction
                  )
                }

                State[N, V](newN, newMean, newM)
            }
          )

      case None =>
        State[N, V](0, ops.zeros(0), ops.zeros(0))
    }

  def batch[D[_]: Data, N: NumericConversion, V[_] <: Vector[_]](
    elems: D[V[N]]
  )(implicit ops: VectorOpsT[N, V]): Stats[V[N]] =
    elems.headOption match {

      case Some(v) =>
        val size = v.size
        apply[D, N, V](State[N, V](0l, ops.zeros(size), ops.zeros(size)), elems).toStats()

      case None =>
        Stats(0, ops.zeros(0), ops.zeros(0))
    }

}