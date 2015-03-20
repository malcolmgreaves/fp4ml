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

object LabeledData {

  @inline def unlabeled[T: ClassTag](example: T): LabeledData[T] =
    LabeledData(noLabel, example)

  @inline def isUnlabeled(x: LabeledData[_]): Boolean =
    x.label == noLabel

  private val noLabel = ""
}