package mlbigbook.ml

import simulacrum._

@typeclass trait Hashable[T] {
  def hash(t: T): Int
}

object ImplicitHashable {

  implicit val bIsH: Hashable[Boolean] = new Hashable[Boolean] {
    @inline override def hash(t: Boolean) = if (t) 1 else 0
  }

  implicit val iIsH: Hashable[Int] = new Hashable[Int] {
    @inline override def hash(t: Int) = t
  }

  implicit val sIsH: Hashable[String] = new Hashable[String] {
    @inline override def hash(t: String) = t.hashCode
  }

  implicit def optIsH[T: Hashable]: Hashable[Option[T]] =
    new Hashable[Option[T]] {
      import Hashable.ops._

      @inline override def hash(maybeT: Option[T]) = maybeT match {
        case Some(t) => t.hash
        case None    => 0
      }
    }

}
