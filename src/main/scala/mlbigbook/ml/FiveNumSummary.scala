package mlbigbook.ml

case class FiveNumSummary[N: Numeric](
    min:    N,
    q1:     N,
    median: N,
    q2:     N,
    max:    N
) {

  override def toString =
    s"[Min: $min, Quartile 1: $q1, Median: $median, Quartile 2: $q2, Max: $max]"
}

object FiveNumSummary {

  def empty[N: Numeric]: FiveNumSummary[N] = {
    val zero = implicitly[Numeric[N]].zero
    FiveNumSummary(zero, zero, zero, zero, zero)
  }
}