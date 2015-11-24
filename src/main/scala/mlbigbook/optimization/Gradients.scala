package mlbigbook.optimization

import breeze.linalg.DenseVector
import mlbigbook.data.{ VectorizedData, DataClass }
import mlbigbook.optimization.Types.GradFn

/**
 * Gradients for various models (e.g. linear regression, logistic regression, neural networks) go here.
 *
 * @author Marek Kolodziej
 */
object Gradients {

  val linearRegressionGradient = GradFn(
    f =
    (data: DataClass[VectorizedData], weights: DenseVector[Double]) => {
      data.aggregate(DenseVector.zeros[Double](weights.iterableSize))(
        seqOp = {

        case (partialGrad: DenseVector[Double], datum) =>
          datum.features.t * (datum.features * weights :- datum.target)
      },
        combOp = {

        case (partVec1, partVec2) => partVec1 :+ partVec2
      }
      )
    }
  )

}