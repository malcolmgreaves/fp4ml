package mlbigbook.util

import breeze.linalg.{ DenseMatrix, DenseVector }
import mlbigbook.data.{ VectorizedData, Datum, DataClass }

/**
 * Various utils for data conversion, etc.
 *
 * @author Marek Kolodziej
 */
object DataConversions {

  /**
   * Convert a Data instance containing individual Datum records to
   * Data containing records grouped into VectorizedData for
   * vectorized linear algebra.
   *
   * @param data
   * @param numExamplesPerGroup
   * @return
   */
  def toVectorizedData(data: DataClass[Datum], numExamplesPerGroup: Int): DataClass[VectorizedData] = {

    val exampleCount = data.size
    val numGroups = exampleCount / numExamplesPerGroup

    val grouped = data.
      zipWithIndex.
      groupBy { case (_, idx) => idx % numGroups }.
      map {
        case (idx, iter) =>
          iter.map {
            case (datum, _) => datum
          }
      }

    grouped.
      map {
        case arr =>
          val numFeat = arr.headOption match {
            case Some(x) => x.features.iterableSize
            case None    => 0
          }
          val init = VectorizedData(
            target = DenseVector.zeros[Double](0),
            features = DenseMatrix.zeros[Double](0, numFeat)
          )
          arr.foldLeft(init)(
            (acc, elem) => {
              val vecCat = DenseVector.vertcat(acc.target, DenseVector(elem.target))
              val featMat = elem.features.toDenseMatrix
              val matCat = DenseMatrix.vertcat(acc.features, featMat)
              VectorizedData(vecCat, matCat)
            }
          )
      }
  }
}
