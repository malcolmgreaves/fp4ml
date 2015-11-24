package mlbigbook.math

import mlbigbook.data.DataClass

/**
 * @author Marek Kolodziej
 */
object FeatureScaling {

  private[this] case class Product3(n: Int, mean: Double, m2: Double)

  def welfordsMethodVariance(elems: DataClass[Double]): Double = {

    val res = elems.aggregate(Product3(0, 0D, 0D))(
      seqOp = {
      case (Product3(n, mean, m2), current) =>
        val newN = n + 1
        val delta = current - mean
        val newMean = mean + delta / newN
        val newM2 = m2 + delta * (current - newMean)
        Product3(newN, newMean, newM2)
    },
      combOp = {
      case (Product3(n1, mean1, m21), Product3(n2, mean2, m22)) => {
        val newN = n1 + n2
        val delta = mean2 - mean1
        val newMean =
          if (n2 * 10 < n1) {
            mean1 + (delta * n2) / (n1 + n2)
          } else if (n1 * 10 < n2) {
            mean2 - (delta * n1) / (n1 + n2)
          } else {
            (mean1 * n1 + mean2 * n2) / (n1 + n2)
          }
        val newM = m21 + m22 + (math.pow(delta, 2) * n1 * n2) / (n1 + n2)
        Product3(newN, newMean, newM)
      }
    }
    )
    if (res.n < 2) 0 else res.m2 / (res.n - 1)
  }
}
