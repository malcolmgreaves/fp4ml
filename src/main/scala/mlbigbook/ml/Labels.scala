package mlbigbook.ml

import mlbigbook.data._

sealed trait Labels {
  def labels: Seq[Labeled]
}

object Labels {

  implicit def distributionIsLabeled(d: LabelDistribution): Labels =
    d match {

      case BinaryLabelDistribution(_, yes, no) =>
        BinaryLabels(yes, no)

      case MultiLabelDistribution(labels, _) =>
        MultiLabels(labels)
    }
}

case class BinaryLabels(yes: Labeled, no: Labeled) extends Labels {
  override val labels = Seq(yes, no)
}

case class MultiLabels(override val labels: Seq[Labeled]) extends Labels