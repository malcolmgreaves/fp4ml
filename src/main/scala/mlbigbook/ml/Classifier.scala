package mlbigbook.ml

import mlbigbook.data._

/** Type representing something that can assign classifications to a type of object. */
trait Classifier[T] extends (T => Labeled)

object Classifier {

  @inline implicit def fn2classifier[T](f: T => Labeled): Classifier[T] =
    new Classifier[T] {
      @inline override def apply(x: T): Labeled = f(x)
    }

  import Vectorizer._

  @inline implicit def unlabeledVectorizer[T](v: Vectorizer[T]): Vectorizer[LabeledData[T]] =
    (ignoreLabelHere: LabeledData[T]) => v(ignoreLabelHere.example)
}