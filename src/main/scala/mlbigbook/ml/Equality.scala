package mlbigbook.ml

import simulacrum.typeclass

import scala.language.{ higherKinds, postfixOps }

@typeclass trait Equality[T] {
  def equalsE(x: T)(y: T): Boolean
  def hashCodeE(x: T): Long
}

object EqualityT {

  def apply[T: Equality]: Equality[T] = implicitly[Equality[T]]

  object Implicits {

    implicit object BooleanE extends Equality[Boolean] {
      @inline override def equalsE(x: Boolean)(y: Boolean) = x == y
      @inline override def hashCodeE(x: Boolean) = x.hashCode().toLong
    }

    implicit object IntE extends Equality[Int] {
      @inline override def equalsE(x: Int)(y: Int) = x == y
      @inline override def hashCodeE(x: Int) = x.toLong
    }

    implicit object LongE extends Equality[Long] {
      @inline override def equalsE(x: Long)(y: Long) = x == y
      @inline override def hashCodeE(x: Long) = x
    }

    implicit object StringE extends Equality[String] {
      @inline override def equalsE(x: String)(y: String) = x == y
      @inline override def hashCodeE(x: String) = x.hashCode.toLong
    }

  }

}