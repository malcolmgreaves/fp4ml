package mlbigbook.ml

import scala.language.{ higherKinds, postfixOps }

import Equality.ops._

case class EqualitySet[T: Equality](
    hashCode2value: Map[Int, T]
) extends Set[T] {

  override def contains(elem: T): Boolean =
    hashCode2value contains elem.hashCodeE

  override def iterator: Iterator[T] =
    hashCode2value.valuesIterator

  override def +(elem: T): Set[T] =
    add(elem)

  def add(elem: T): EqualitySet[T] = {
    val hash = elem.hashCodeE

    if (!(hashCode2value contains elem.hashCodeE)) {
      val added = hashCode2value + (hash -> elem)
      EqualitySet(added)

    } else
      this
  }

  override def -(elem: T): Set[T] =
    remove(elem)

  def remove(elem: T): EqualitySet[T] = {
    val hash = elem.hashCodeE

    if (hashCode2value contains elem.hashCodeE) {
      val removed = hashCode2value - hash
      EqualitySet(removed)

    } else
      this
  }

  val equality: Equality[T] = implicitly[Equality[T]]

}

object EqualitySet {

  def apply[T: Equality](elems: T*): Set[T] =
    EqualitySet(
      elems
      .map { e => (e.hashCodeE, e) }
      .toMap
    )

  def empty[T: Equality]: Set[T] =
    EqualitySet(Map.empty[Int, T])

}