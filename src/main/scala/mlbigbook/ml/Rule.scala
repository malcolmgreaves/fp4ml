package mlbigbook.ml

abstract class Rule[N: Numeric] {

  /**
   * The discretization function. It evaluates to a discretized value for the
   * input numerical value.
   */
  def apply(value: N): String

  /**
   * The Strings associated with each interval. These elements are ordered
   * according to the ascending numerical ranges of each associated interval.
   */
  def discretizedValueBases: Seq[String]
}

object Rule {

  /**
   * Evaluates to a Rule instance using the input threshold and label
   * sequence.
   *
   * The input thresholds sequence must be in ascending order on the threshold
   * value.
   *
   * The evaluated Rule's apply method evaluates to the String associated
   * with the first threshold value that the input numerical value is less
   * than. If the value is larger than or equal to all thresholds, then the
   * defaultTopInterval String is the evaluated result.
   */
  def apply[N: Numeric](
    thresholds:         Seq[(N, String)],
    defaultTopInterval: String
  ): Rule[N] =
    new Rule[N] {

      val lessThan = implicitly[Numeric[N]].lt _

      override def apply(value: N): String =
        thresholds.foldLeft(defaultTopInterval) {
          case (choice, (threshold, intervalName)) =>
            if (lessThan(value, threshold))
              intervalName
            else
              choice
        }

      override val discretizedValueBases =
        thresholds.map { case (_, name) => name } :+ defaultTopInterval
    }

  /**
   * Evaluates to a Rule instance whose apply always evaluates to the empty
   * String. And whose discretizedValueBases is a one element sequence, which
   * only contains the empty String.
   */
  def empty[N: Numeric]: Rule[N] =
    new Rule[N] {
      override def apply(value: N) = ""
      override val discretizedValueBases = Seq("")
    }

}