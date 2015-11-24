package mlbigbook.wordcount

import mlbigbook.data.DataClass

object GenericCount {

  def empty[T, N: Numeric]: Map[T, N] =
    Map.empty[T, N]

  @inline final def increment[T, N: Numeric](map: Map[T, N], e: T): Map[T, N] =
    increment(map, e, implicitly[Numeric[N]].one)

  final def increment[T, N: Numeric](map: Map[T, N], e: T, times: N): Map[T, N] =
    if (map contains e)
      (map - e) + (e -> implicitly[Numeric[N]].plus(map(e), times))
    else
      map + (e -> times)

  final def increment[T, N: Numeric](map: Map[T, N], es: DataClass[T]): Map[T, N] =
    es.aggregate(map)(
      { case (m, e) => increment(m, e) },
      { case (m1, m2) => combine(m1, m2) }
    )

  /**
   * Combines two maps. If maps m1 and m2 both have key k, then the resulting
   * map will have m1(k) + m2(k) for the value of k.
   */
  final def combine[T, N: Numeric](m1: Map[T, N], m2: Map[T, N]): Map[T, N] = {
    val (smaller, larger) = if (m1.size < m2.size) (m1, m2) else (m2, m1)
    smaller.foldLeft(larger) {
      case (aggmap, (k, v)) =>
        aggmap.get(k) match {
          case Some(existing) =>
            (aggmap - k) + (k -> implicitly[Numeric[N]].plus(existing, v))
          case None =>
            aggmap + (k -> v)
        }
    }
  }

}