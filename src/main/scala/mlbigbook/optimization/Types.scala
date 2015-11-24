package mlbigbook.optimization

import breeze.linalg.DenseVector
import mlbigbook.data.{ VectorizedData, DataClass }

/**
 * Common types used throughout the optimization code base.
 *
 * @author Marek Kolodziej
 */
object Types {

  case class GradFn(f: (DataClass[VectorizedData], DenseVector[Double]) => DenseVector[Double]) {
    def apply(data: DataClass[VectorizedData], weights: DenseVector[Double]) =
      f(data, weights)
  }

  case class CostFn(f: (DataClass[VectorizedData], DenseVector[Double]) => Double) {
    def apply(data: DataClass[VectorizedData], weights: DenseVector[Double]) =
      f(data, weights)
  }

  case class WeightUpdate(f: (DataClass[VectorizedData], OptHistory, GradFn, CostFn, Double, Double, Double, Int, Long) => OptHistory) {
    def apply(
      data:              DataClass[VectorizedData],
      history:           OptHistory,
      gradFn:            GradFn,
      costFn:            CostFn,
      initAlpha:         Double,
      momentum:          Double,
      miniBatchFraction: Double,
      miniBatchIterNum:  Int,
      seed:              Long
    ): OptHistory =
      f(
        data, history, gradFn, costFn, initAlpha,
        momentum, miniBatchFraction, miniBatchIterNum, seed
      )
  }

  case class WeightInit(f: (Int, Long) => DenseVector[Double]) {
    def apply(numEl: Int, seed: Long): DenseVector[Double] =
      f(numEl, seed)
  }

}
