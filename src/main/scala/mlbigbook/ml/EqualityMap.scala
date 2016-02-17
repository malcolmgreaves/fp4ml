package mlbigbook.ml

import scala.language.{ higherKinds, postfixOps }

import Equality.ops._

case class EqualityMap[K: Equality, V](
    hashCode2value: Map[Long, V],
    keysE:          EqualitySet[K]
) extends Map[K, V] {

  override def +[B1 >: V](kv: (K, B1)): Map[K, B1] = {
    val (key, value) = kv
    val hash = key.hashCodeE
    EqualityMap(
      hashCode2value + (hash -> value),
      keysE.add(key)
    )
  }

  override def -(key: K): Map[K, V] = {
    val hash = key.hashCodeE
    EqualityMap(
      hashCode2value - hash,
      keysE.remove(key)
    )
  }

  override def get(key: K): Option[V] =
    hashCode2value get (key hashCodeE)

  override def iterator: Iterator[(K, V)] =
    keys
      .map { key =>
        val hash = key.hashCodeE
        (key, hashCode2value(hash))
      }
      .toIterator

  val equality: Equality[K] = implicitly[Equality[K]]

}

object EqualityMap {

  def apply[K: Equality, V](kvs: (K, V)*): Map[K, V] = {
    val kvm: Map[K, V] = kvs.toMap
    val keyHash2value = kvm.map { case (key, value) => (key.hashCodeE, value) }
    val keyHash2key = kvm.map { case (key, _) => (key.hashCodeE, key) }
    EqualityMap(keyHash2value, EqualitySet(keyHash2key))
  }

  def empty[K: Equality, V]: Map[K, V] =
    EqualityMap(Map.empty[Long, V], EqualitySet(Map.empty[Long, K]))

}