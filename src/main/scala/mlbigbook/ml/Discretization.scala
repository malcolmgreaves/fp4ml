package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }
import breeze.linalg.Vector

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

trait Discretization {

  import Discretization._

  def apply[D[_]: Data, V[_] <: Vector[_], N: NumericConversion: ClassTag](
    data: D[V[N]]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): (D[DiscretizedVector], FeatureSpace)

}

object Discretization {

  type DiscretizedVector = Seq[String]

  def newCategoricalFs(
    discretizedValueBasesForEach: Seq[String]
  )(implicit fs: FeatureSpace): FeatureSpace =

    FeatureSpace.allCategorical(
      fs.features,
      fs.features
      .map { feature =>
        (
          feature,
          discretizedValueBasesForEach.map { dv => s"$feature--$dv" }
        )
      }
      .toMap
    )

}