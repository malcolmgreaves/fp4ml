/*
 * Types and implementations to represent labeled data.
 *
 * @author Malcolm Greaves
 */
package mlbigbook.data

import mlbigbook.data

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
case class LabeledData[T](label: String, example: T) extends Labeled
