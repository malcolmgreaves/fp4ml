package mlbigbook.optimization

import breeze.linalg.DenseVector
import mlbigbook.data.{ VectorizedData, DataClass }
import mlbigbook.optimization.Types.CostFn

/**
 * Cost functions for various models (e.g. linear regression, logistic regression, neural networks) go here.
 *
 * @author Marek Kolodziej
 */
object CostFunctions {

  val linearRegressionCost = CostFn(
    (data: DataClass[VectorizedData], weights: DenseVector[Double]) => {

      val counts = data.map(_.target.activeSize).reduceLeft(_ + _)

      val unscaledCost =
        data
          .aggregate(0.0)(
            seqOp = {
            case (currCost, elem) =>
              currCost + (elem.features * weights :- elem.target)
                .map(i => math.pow(i, 2))
                .reduceLeft(_ + _)
          },
            combOp = {
            case (a, b) => a + b
          }
          )

      unscaledCost / (2 * counts)
    }
  )

}