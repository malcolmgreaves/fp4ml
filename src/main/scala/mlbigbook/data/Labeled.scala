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

  final override def equals(o: Any) =
    o match {

      case that: Labeled =>
        that.label == label

      case _ =>
        false
    }

  final override def hashCode =
    label.hashCode

  override def toString =
    label
}

object Labeled {

  def apply(l: String): Labeled =
    L(l)

  private case class L(override val label: String) extends Labeled
}

/** Represents a piece of labeled data. */
case class LabeledData[T: ClassTag](label: String, example: T) extends Labeled

object UnlabeledData {

  private val unlabeledStr = ""

  @inline def apply[T: ClassTag](example: T): LabeledData[T] =
    LabeledData(unlabeledStr, example)

  @inline def isA(ld: LabeledData[_]): Boolean =
    unlabeledStr == ld.label

  import Vectorizer._

  @inline implicit def vectorizer[T](v: Vectorizer[T]): Vectorizer[LabeledData[T]] =
    (ignoreLabelHere: LabeledData[T]) => v(ignoreLabelHere.example)

  val asLabled = Labeled(unlabeledStr)

}