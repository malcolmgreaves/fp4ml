package mlbigbook.ml

import scala.annotation.tailrec

final class CustomHashMap[K: Hashable, V](
    private[this] val hashedKey2val: Map[Int, V],
    private[this] val hashedKeys: List[K]
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
    hashedKeys.toIterator.map { key =>
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
      before: List[K],
      remaining: List[K]
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
