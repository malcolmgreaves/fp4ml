package mlbigbook.ml

import mlbigbook.data._

/** Type representing something that can assign classifications to a type of object. */
trait Classifier[T] extends (T => Labeled)

object Classifier {

  implicit class Fn[T](val f: T => Labeled) extends Classifier[T] {
    override def apply(x: T) = f(x)
  }

}