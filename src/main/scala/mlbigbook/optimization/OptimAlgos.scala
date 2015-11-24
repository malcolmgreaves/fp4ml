package mlbigbook.optimization

import breeze.linalg.DenseVector
import mlbigbook.data.{ DataClass, VectorizedData }
import mlbigbook.optimization.Types.{ CostFn, GradFn, WeightUpdate }
import mlbigbook.util.Sampling.sampleMiniBatch

/**
 * The optimizization algorithms (SGD, Adagrad, L-BFGS, etc.) go here.
 *
 * @author Marek Kolodziej
 */
object OptimAlgos {

  // helper class to make the SGD and Adagrad code more DRY, since this is repetitive stuff
  private case class OptInfo(
      private val data:              DataClass[VectorizedData],
      private val miniBatchFraction: Double,
      private val currSeed:          Long,
      private val history:           OptHistory,
      private val costFn:            CostFn,
      private val gradFn:            GradFn
  ) {

    val weights = history.weights.last
    private val histLen = history.cost.size
    lazy val sample = sampleMiniBatch(data, miniBatchFraction, currSeed)
    lazy val sampleSize = sample.map(_.target.activeSize).reduceLeft(_ + _)
    lazy val newCost = costFn(sample, weights)
    lazy val gradients = gradFn(sample, weights)
    lazy val prevDeltaW = history.weights(histLen - 1) :- history.weights(histLen - 2)
  }

  /* stochastic gradient descent
     see http://leon.bottou.org/publications/pdf/online-1998.pdf
   */
  val sgd = WeightUpdate(
    f = (data: DataClass[VectorizedData],
    history: OptHistory,
    gradFn: GradFn,
    costFn: CostFn,
    initAlpha: Double,
    momentum: Double,
    miniBatchFraction: Double,
    miniBatchIterNum: Int,
    seed: Long) => {

    val opt = OptInfo(data, miniBatchFraction, seed + miniBatchIterNum, history, costFn, gradFn)
    val eta = initAlpha / math.sqrt(opt.sampleSize * miniBatchIterNum)
    val mom: DenseVector[Double] = opt.prevDeltaW :* momentum
    val newWtsNoMom: DenseVector[Double] = opt.weights :- (opt.gradients :* eta)
    val gradWithMom = (opt.gradients :* eta) :+ mom
    val newWtsWithMom = newWtsNoMom :+ mom
    OptHistory(
      cost = history.cost :+ opt.newCost,
      weights = history.weights :+ newWtsWithMom,
      grads = history.grads :+ gradWithMom
    )
  }
  )

  /* Adagrad
     see http://www.jmlr.org/papers/volume12/duchi11a/duchi11a.pdf
   */
  val adaGrad = WeightUpdate(
    f = (data: DataClass[VectorizedData],
    history: OptHistory,
    gradFn: GradFn,
    costFn: CostFn,
    initAlpha: Double,
    momentum: Double,
    miniBatchFraction: Double,
    miniBatchIterNum: Int,
    seed: Long) => {

    val opt = OptInfo(data, miniBatchFraction, seed + miniBatchIterNum, history, costFn, gradFn)
    val mom: DenseVector[Double] = opt.prevDeltaW :* momentum
    val adaGradDiag: DenseVector[Double] =
      history.grads.foldLeft(
        DenseVector.zeros[Double](opt.weights.iterableSize)
      )(
          (acc: DenseVector[Double], item: DenseVector[Double]) => {
            val temp: Array[Double] = acc.toArray.zip(item.toArray).map(i => i._1 + math.pow(i._2, 2))
            new DenseVector[Double](temp)
          }
        )
    val scaledByDiag = new DenseVector[Double](
      opt.gradients.toArray.zip(adaGradDiag.toArray).map(
        i =>
          initAlpha * i._1 / math.sqrt(i._2)
      )
    )
    val adaGradWts = (opt.weights :- scaledByDiag) :+ mom
    OptHistory(
      cost = history.cost :+ opt.newCost,
      weights = history.weights :+ adaGradWts,
      grads = history.grads :+ scaledByDiag
    )
  }
  )
}
