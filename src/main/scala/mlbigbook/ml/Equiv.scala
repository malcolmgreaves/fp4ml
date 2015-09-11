package mlbigbook.ml

import mlbigbook.ml.Equiv.Implicits.OverrideEqualsHashCode

import scala.reflect.ClassTag

/**
 * Implementation of naive Bayes for discrete, event-based features.
 */
trait Equiv[A] {

  def equiv(a: A, b: A): Boolean

  def hashCode(a: A): Int
}

object Equiv {

  def apply[A:Equiv]: Equiv[A] =
    implicitly[Equiv[A]]

  object Implicits {

    implicit final class OverrideEqualsHashCode[A:Equiv](instance: A) {

      private[this] val e = Equiv[A]

      override def equals(anything: Any): Boolean =
        if(instance == null)
          anything == null
        else
          try {
            e.equiv(instance, anything.asInstanceOf[A])
          } catch {
            case _: ClassCastException => false
          }

      override def hashCode: Int =
        e.hashCode(instance)
    }

  }
}