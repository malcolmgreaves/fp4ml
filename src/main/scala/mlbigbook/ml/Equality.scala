package mlbigbook.ml

import simulacrum.typeclass

import scala.language.{ higherKinds, postfixOps }

@typeclass trait Equality[T] {
  def equalsE(x: T)(y: T): Boolean
  def hashCodeE(x: T): Int
}

