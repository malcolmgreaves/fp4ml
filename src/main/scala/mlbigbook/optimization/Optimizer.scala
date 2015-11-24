package mlbigbook.optimization

import mlbigbook.data.{ VectorizedData, DataClass }
import mlbigbook.optimization.Types.{ WeightInit, WeightUpdate, CostFn, GradFn }
import org.apache.log4j.Logger

/**
 * @author Marek Kolodziej
 */
object Optimizer {

  @transient val log = Logger.getLogger(Optimizer.getClass)

  def optimize(
    iter:              Int,
    seed:              Long                      = 42L,
    initAlpha:         Double                    = 0.1,
    momentum:          Double                    = 0.0,
    gradFn:            GradFn,
    costFn:            CostFn,
    updateFn:          WeightUpdate,
    miniBatchFraction: Double,
    weightInitializer: WeightInit,
    data:              DataClass[VectorizedData]
  ): OptHistory = {

    val count = data.size
    val dataSize = data.headOption match {
      case Some(x) => x.features.cols
      case None    => 0
    }
    val exampleCount = data.map(i => i.target.activeSize).reduceLeft(_ + _)
    val initWts = weightInitializer(dataSize, seed)
    val initGrads = weightInitializer(dataSize, seed + 1)
    val initCost = costFn(data, initWts) / (miniBatchFraction * exampleCount)
    // we need 2 steps of history at initialization time for momentum to work correctly
    val initHistory = OptHistory(cost = Seq(initCost, initCost), weights = Seq(initWts, initWts), grads = Seq(initGrads, initGrads))

    (1 to iter).foldLeft(initHistory) {

      case (history, it) =>

        if (it == iter)
          history
        else
          updateFn(data, history, gradFn, costFn, initAlpha, momentum, miniBatchFraction, it, seed)
    }
  }
}