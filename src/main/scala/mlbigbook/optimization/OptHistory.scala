package mlbigbook.optimization

import breeze.linalg.DenseVector

/**
 * Container for optimization history. This is useful for debugging and plotting
 * (historical cost and weight values), but also for the optimizer itself
 * (e.g. gradient history for the Adagrad optimizer).
 *
 * @author Marek Kolodziej
 *
 * @param cost
 * @param weights
 * @param grads
 */
case class OptHistory(
                       cost: Seq[Double],
                       weights: Seq[DenseVector[Double]],
                       grads: Seq[DenseVector[Double]]
                     )
