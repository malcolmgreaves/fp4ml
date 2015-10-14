package mlbigbook.ml

import mlbigbook.data._

sealed trait LabelDistribution {

  /**
   * The `labels` contains descriptions for for each probability in the distribution.
   * Align 1-to-1 with `values`.
   */
  def labels: Seq[Labeled]

  /**
   * The probability values of this distribution. Align 1-to-1 with labels.
   */
  def values: Seq[Double]

  /**
   * The number of elements in the distribution.
   *
   * The evaluated result is equivalent to labels.size and values.size
   */
  def size: Int
}

object LabelDistribution {

  type Maker = Labels => Seq[Double] => Option[LabelDistribution]

  def make: Maker =
    (ls: Labels) =>
      ls match {

        case BinaryLabels(yes, no) =>
          (p: Seq[Double]) =>
            if (p.size == 2)
              Some(BinaryLabelDistribution(p.head, yes, no))
            else
              None

        case MultiLabels(mLabels) =>
          (p: Seq[Double]) =>
            if (p.size == mLabels.size)
              Some(MultiLabelDistribution(mLabels, p))
            else
              None
      }
}

case class BinaryLabelDistribution(
    yesProbability: Double,
    yesLabel:       Labeled,
    noLabel:        Labeled
) extends LabelDistribution {

  val noProbability =
    1.0 - yesProbability

  override val values =
    Seq(yesProbability, noProbability)

  override val labels =
    Seq(yesLabel, noLabel)

  override val size =
    2

  override def toString =
    s"[$yesLabel: $yesProbability , $noLabel : $noProbability]"
}

case class MultiLabelDistribution(
    override val labels: Seq[Labeled],
    override val values: Seq[Double]
) extends LabelDistribution {

  assert(labels.size == values.size)

  override val size =
    labels.size

  override def toString =
    labels.zip(values)
      .map { case (l, v) => s"($l : $v)" }
      .mkString("\n")
}