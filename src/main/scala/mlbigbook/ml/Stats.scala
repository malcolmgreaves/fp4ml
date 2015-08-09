package mlbigbook.ml

case class Stats[@specialized V](count: Long, mean: V, variance: V) {

  override def toString =
    s"$count elements, with mean: $mean and variance: $variance"
}