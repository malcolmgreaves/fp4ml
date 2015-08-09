package mlbigbook.util

import mlbigbook.data.Data
import mlbigbook.ml.VectorOpsT

import breeze.linalg.Vector

/**
 * @author Marek Kolodziej
 * @author Malcolm Greaves
 */
object OnlineMeanVariance {

  case class Stats[V](count: Long, mean: V, variance: V)

  private[this] case class State[V](count: Long, mean: V, m2: V)

  /**
   * Uses Welford's Method to calculate variance.
   */
  def apply[N : NumericConversion, V[_] <: Vector[_]](
    elems: Data[V[N]]
  )(implicit ops: VectorOpsT[N, V]): Stats[V[N]] =
    elems.headOption match {

      case Some(v) =>
        val size = v.size
        val numConv = implicitly[NumericConversion[N]]

        val res =
          elems
            .aggregate(State(0l, ops.zeros(size), ops.zeros(size)))(
              {
                case (State(n, mean, m2), current) =>
                  val newN = n + 1l
                  val delta = ops.subV(current, mean)
                  val newMean = ops.addV(mean, ops.divS(delta, numConv.fromLong(n)))
                  val newM2 = ops.addV(m2, ops.mulV(delta, ops.subV(current, newMean)))
                  State(newN, newMean, newM2)
              },
              {
                case (State(n1, mean1, m21), State(n2, mean2, m22)) =>
                  val newN = n1 + n2
                  val delta = ops.subV(mean2, mean1)
                  val newMean =
                    if (n2 * 10l < n1)
                      //                      mean1 + (delta * n2) / (n1 + n2)
                      ops.addV(mean1, ops.divS(ops.mulS(delta, numConv.fromLong(n2)), numConv.fromLong(newN)))
                    else if (n1 * 10 < n2)
                      //                      mean2 - (delta * n1) / (n1 + n2)
                      ops.subV(mean2, ops.divS(ops.mulS(delta, numConv.fromLong(n1)), numConv.fromLong(newN)))
                    else
                      //                      (mean1 * n1 + mean2 * n2) / (n1 + n2)
                      ops.divS(ops.addV(ops.mulS(mean1, numConv.fromLong(n1)), ops.mulS(mean2, numConv.fromLong(n2))), numConv.fromLong(newN))

                  //                  val newM = m21 + m22 + (math.pow(delta, 2) * n1 * n2) / (n1 + n2)
                  val newM = {
                    val deltaSquared = ops.mulV(delta, delta)
                    val fraction =
                      ops.divS(
                        ops.mulS(ops.mulS(deltaSquared, numConv.fromLong(n1)), numConv.fromLong(n2)),
                        numConv.fromLong(newN)
                      )
                    ops.addV(ops.addV(m21, m22), fraction)
                  }

                  State(newN, newMean, newM)
              }
            )

        Stats(
          res.count,
          res.mean,
          if (res.count < 2l)
            ops.zeros(size)
          else
            ops.divS(res.m2, numConv.fromLong(res.count - 1l))
        )

      case None =>
        Stats(0, ops.zeros(0), ops.zeros(0))
    }

}
