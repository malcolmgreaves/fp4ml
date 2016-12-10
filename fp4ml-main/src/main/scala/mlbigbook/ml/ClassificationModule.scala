package mlbigbook.ml

import fif.Data
import simulacrum.typeclass

import scala.annotation.tailrec
import scala.language.{ higherKinds, postfixOps, reflectiveCalls }
import scala.reflect.ClassTag

trait ClassificationModule extends ItemNumVecModule {

  type Label
  val emptyLabel: Label

  type Classifier = Item => Label

  type Vectorizer = {
    val vectorize: Item => V[N]
    val nDimensions: Int
  }

  type Conf

  import Data.ops._

  final def train[D[_]: Data](
    c:            Conf,
    mkVectorizer: D[(Item, Label)] => Vectorizer
  )(data: D[(Item, Label)]): Classifier =
    train(
      c,
      mkVectorizer { data }
    )(data)

  def train[D[_]: Data](
    c:     Conf,
    toVec: Vectorizer
  )(data: D[(Item, Label)]): Classifier

}

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

final class CustomHashMap[K: Hashable, V](
    private[this] val hashedKey2val: Map[Int, V],
    private[this] val hashedKeys:    List[K]
) extends Map[K, V] {

  override def +[B1 >: V](kv: (K, B1)): Map[K, B1] = {
    val (key, value) = kv
    val id = implicitly[Hashable[K]].hash(key)

    if (hashedKey2val contains id)
      new CustomHashMap(
        (hashedKey2val - id) + (id -> value),
        hashedKeys
      )
    else
      new CustomHashMap(
        hashedKey2val + (id -> value),
        hashedKeys :+ key
      )
  }

  override def get(key: K): Option[V] = {
    val id = implicitly[Hashable[K]].hash(key)
    hashedKey2val.get(id)
  }

  override def iterator: Iterator[(K, V)] =
    hashedKeys
      .toIterator
      .map { key =>
        val id = implicitly[Hashable[K]].hash(key)
        (key, hashedKey2val(id))
      }

  override def -(key: K): Map[K, V] = {
    val id = implicitly[Hashable[K]].hash(key)
    if (hashedKey2val contains id)
      new CustomHashMap(
        hashedKey2val - id,
        remove(id, hashedKeys, Nil)
      )
    else
      this
  }

  @tailrec
  private[this] def remove(
    idOfKeyToRemove: Int,
    before:          List[K],
    remaining:       List[K]
  ): List[K] =
    remaining match {

      case anotherKey :: restOfList =>
        val idOfAnother = implicitly[Hashable[K]].hash(anotherKey)
        if (idOfAnother == idOfKeyToRemove)
          before ++ restOfList
        else
          remove(idOfKeyToRemove, before :+ anotherKey, restOfList)

      case Nil =>
        before
    }

}

object CustomHashMap {

  def empty[K: Hashable, V]: Map[K, V] =
    new CustomHashMap[K, V](Map.empty[Int, V], Nil)

}

