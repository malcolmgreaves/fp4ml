package mlbigbook.ml

import fif.Data
import Data.ops._

import scala.language.higherKinds
import scala.reflect.ClassTag

object Counting {

  def empty[T, N: Numeric]: Map[T, N] =
    Map.empty[T, N]

  def increment[T, N: Numeric](map: Map[T, N], e: T): Map[T, N] =
    increment(map, e, implicitly[Numeric[N]].one)

  def increment[T, N: Numeric](map: Map[T, N], e: T, times: N): Map[T, N] =
    if (map contains e)
      (map - e) + (e -> implicitly[Numeric[N]].plus(map(e), times))
    else
      map + (e -> times)

  def incrementNested[K1, K2, N: Numeric](
    map:  Map[K1, Map[K2, N]],
    key1: K1,
    key2: K2
  ): Map[K1, Map[K2, N]] =
    if (map contains key1)
      (map - key1) + (key1 -> increment[K2, N](map(key1), key2))
    else
      map + (key1 -> Map(key2 -> implicitly[Numeric[N]].one))

  def incrementNested[K1, K2, N: Numeric](
    map:   Map[K1, Map[K2, N]],
    key1:  K1,
    key2:  K2,
    times: N
  ): Map[K1, Map[K2, N]] =
    if (map contains key1)
      (map - key1) + (key1 -> increment(map(key1), key2, times))
    else
      map + (key1 -> Map(key2 -> times))

  def count[T, N: Numeric, D[_]: Data](data: D[T]): Map[T, N] =
    count(empty, data)

  def count[T, N: Numeric, D[_]: Data](existingMap: Map[T, N], data: D[T]): Map[T, N] =
    data.aggregate(existingMap)(increment[T, N], combine[T, N])

  /**
   * Combines two maps. If maps m1 and m2 both have key k, then the resulting
   * map will have m1(k) + m2(k) for the value of k.
   */
  def combine[T, N: Numeric](m1: Map[T, N], m2: Map[T, N]): Map[T, N] = {
    val (smaller, larger) =
      if (m1.size < m2.size)
        (m1, m2)
      else
        (m2, m1)

    smaller.foldLeft(larger) {
      case (aggmap, (k, v)) =>

        if (aggmap contains k)
          (aggmap - k) + (k -> implicitly[Numeric[N]].plus(aggmap(k), v))

        else
          aggmap + (k -> v)
    }
  }

  def combineNested[K1, K2, N: Numeric](
    m1: Map[K1, Map[K2, N]],
    m2: Map[K1, Map[K2, N]]
  ): Map[K1, Map[K2, N]] = {
    val (smaller, larger) =
      if (m1.size < m2.size)
        (m1, m2)
      else
        (m2, m1)

    smaller.foldLeft(larger) {
      case (aggmap, (k1, submap)) =>

        if (aggmap contains k1)
          (aggmap - k1) + (k1 -> combine(aggmap(k1), submap))

        else
          aggmap + (k1 -> submap)
    }
  }

}
