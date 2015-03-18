package mlbigbook.ml

import mlbigbook.data._

/** Type representing something that can assign classifications to a type of object. */
trait Classifier[T] extends (T => Labeled)

object Classifier {

  implicit def fn2classifier[T](f: T => Labeled): Classifier[T] =
    new Classifier[T] {
      override def apply(x: T): Labeled = f(x)
    }
}