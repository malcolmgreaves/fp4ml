package mlbigbook.ml

import fif.Data
import mlbigbook.math.VectorOpsT
import mlbigbook.ml.FeatureVectorSupport.FeatureSpace
import simulacrum.typeclass
import breeze.linalg.Vector

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

trait Discretization {

  import Discretization._

  def apply[D[_]: Data, V[_] <: Vector[_], N: Numeric: ClassTag](
    data:    D[V[N]],
    headers: Seq[String]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): (D[DiscretizedVector], ValuesPerFeature)

}

object Discretization {

  type ValuesPerFeature = Seq[Seq[String]]
  type DiscretizedVector = Seq[String]

}