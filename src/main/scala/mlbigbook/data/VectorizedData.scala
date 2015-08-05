package mlbigbook.data

import breeze.linalg.{ DenseVector, DenseMatrix }

/**
 * Group a bunch of examples into a feature matrix and a target vector,
 * instead of processing a feature vector and a target value at a time.
 * This will allow for vectorizing the linear algebra. When Breeze's
 * BLAS support is available (see https://github.com/fommil/netlib-java),
 * Breeze will execute linear algebra operations natively, benefiting from
 * lack of garbage collection, vectorization via SSE, etc.
 *
 * @author Marek Kolodziej
 *
 * @param target
 * @param features
 */
case class VectorizedData(target: DenseVector[Double], features: DenseMatrix[Double]) {
  override def toString: String =
    s"""VectorizedData(
       |target = $target,
       |features = $features
       |)
       """.stripMargin
}

/**
 * Wrapper for a single example (target and features)
 *
 * @author Marek Kolodziej
 *
 * @param target
 * @param features
 */
case class Datum(target: Double, features: DenseVector[Double]) {
  override def toString: String =
    s"Datum(target = $target, features = $features)"
}
