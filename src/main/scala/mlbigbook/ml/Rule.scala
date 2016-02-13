package mlbigbook.ml

abstract class Rule[N: Numeric] {

  def apply(value: N): String

  def discretizedValueBases: Seq[String]
}

object Rule {

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

}