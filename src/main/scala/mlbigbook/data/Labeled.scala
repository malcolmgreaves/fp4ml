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

  implicit def str2labeled(s: String): Labeled =
    new Labeled {
      override val label = s
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