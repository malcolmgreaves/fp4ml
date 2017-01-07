package mlbigbook.math

import fif.Data

import scala.reflect.ClassTag

object Argmin {

  def apply[T: Val: ClassTag, D[_]: Data](elements: D[T]): Option[T] =
    Argmax(elements)(
      Val.inverse,
      implicitly[ClassTag[T]],
      implicitly[Data[D]]
    )

  def applyUnsafe[T: Val: ClassTag, D[_]: Data](elements: D[T]): T =
    Argmax.applyUnsafe(elements)(
      Val.inverse,
      implicitly[ClassTag[T]],
      implicitly[Data[D]]
    )
}
