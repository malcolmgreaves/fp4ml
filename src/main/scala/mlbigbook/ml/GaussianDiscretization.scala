package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import fif.Data.ops._
import mlbigbook.math.{ NumericConversion, OnlineMeanVariance, VectorOpsT }

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object GaussianDiscretization extends Discretization {

  override def apply[D[_]: Data, V[_] <: Vector[_], N: NumericConversion: ClassTag](
    data: D[V[N]]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ) = {

    val statsForEachFeature = OnlineMeanVariance.batch(data)

    //    statsForEachFeature.mean

    ???
  }

}