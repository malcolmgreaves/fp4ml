package mlbigbook.data

import breeze.math.Semiring
import breeze.storage.Zero
import fif.Data
import mlbigbook.math.MathVectorOps

import scala.language.higherKinds

trait OLD_MkVectorizer {

  def apply[D[_]: Data, N: Numeric: Semiring: Zero, V[_], T](
    data: D[T]
  )(
    implicit
    vops: MathVectorOps[N, V]
  ): T => V[N]

}