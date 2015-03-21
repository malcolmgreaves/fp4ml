/*
 * Types and implementations to represent labeled data.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import scala.reflect.ClassTag

/** Represents something that has a label. */
trait Labeled {
  def label: String
}

object Labeled {

  def apply(l: String): Labeled =
    new Labeled {
      override val label = l
    }
}

/** Represents a piece of labeled data. */
case class LabeledData[T: ClassTag](label: String, example: T) extends Labeled

object UnlabeledData {

  @inline def apply[T: ClassTag](example: T): LabeledData[T] =
    LabeledData("", example)

  @inline def isA(ld: LabeledData[_]): Boolean =
    "" == ld.label

  import Vectorizer._

  @inline implicit def vectorizer[T](v: Vectorizer[T]): Vectorizer[LabeledData[T]] =
    (ignoreLabelHere: LabeledData[T]) => v(ignoreLabelHere.example)

}