package mlbigbook.math

import breeze.math.Semiring
import breeze.storage.Zero

import scala.language.higherKinds

protected[math] abstract class BaseMvo[N, V[_]](
    implicit
    no: Numeric[N],
    zo: Zero[N],
    so: Semiring[N]
) extends MathVectorOps[N, V] {

  override final implicit val n = no
  override final implicit val z = zo
  override final implicit val s = so

}