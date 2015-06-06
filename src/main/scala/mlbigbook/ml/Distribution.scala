package mlbigbook.ml

import mlbigbook.data._

sealed trait Distribution {

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

object DistributionMaker {

  type Maker = Labels => Seq[Double] => Option[Distribution]

  def apply: Maker =
    (ls: Labels) =>
      ls match {

        case BinaryLabels(yes, no) =>
          (p: Seq[Double]) =>
            if (p.size == 2)
              Some(BinaryDistribution(p.head, yes, no))
            else
              None

            case MultiLabels(mLabels) =>
          (p: Seq[Double]) =>
            if (p.size == mLabels.size)
              Some(MultiDistribution(mLabels, p))
            else
              None
      }
}

case class BinaryDistribution(
    yesProbability: Double,
    yesLabel: Labeled,
    noLabel: Labeled) extends Distribution {

  val noProbability =
    1.0 - yesProbability

  override val values =
    Seq(yesProbability, noProbability)

  override val labels =
    Seq(yesLabel, noLabel)

  override val size =
    2
}

case class MultiDistribution(
    override val labels: Seq[Labeled],
    override val values: Seq[Double]) extends Distribution {

  assert(labels.size == values.size)

  override val size =
    labels.size
}