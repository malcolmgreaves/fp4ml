package mlbigbook.math

import breeze.math.Semiring
import breeze.storage.Zero

import scala.language.higherKinds

private[math] abstract class BaseMathVecOps[Num, V[_]](
    implicit no: Fractional[Num],
    zo: Zero[Num],
    so: Semiring[Num]
) extends MathVectorOps[V] {

  final override type N = Num

  override final implicit lazy val n = no
  override final implicit lazy val z = zo
  override final implicit lazy val s = so

}
