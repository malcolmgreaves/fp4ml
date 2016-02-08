package mlbigbook.ml

import fif.Data
import mlbigbook.math.VectorOpsT
import simulacrum.typeclass
import breeze.linalg.Vector

import scala.language.{ higherKinds, postfixOps }

@typeclass trait Discretization[T] {

  def discretize[D[_]: Data, V[_] <: Vector[_], N: Numeric](
    t: T
  )(
    data:    D[V[N]],
    headers: Seq[String]
  )(
    implicit
    vops: VectorOpsT[N, V]
  ): (D[Seq[String]], Seq[Seq[String]])

}