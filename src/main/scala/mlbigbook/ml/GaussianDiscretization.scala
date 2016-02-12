package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object GaussianDiscretization extends RuleProducer {

  override def apply[D[_]: Data, V[_] <: Vector[_], N: NumericConversion: MathOps: ClassTag](
    data: D[V[N]]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): Seq[Rule[N]] = {

    implicit val _ = NumericConversion[N].numeric
    Gaussian.estimate(data)
      .map(g => gaussianRule(g))
  }

  val below_neg3_sdev = "below_neg_3_sdev"
  val between_neg3_inclusive_and_neg2_exclusive = "between_neg3_inclusive_and_neg2_exclusive"
  val between_neg2_inclusive_and_neg1_exclusive = "between_neg2_inclusive_and_neg1_exclusive "
  val between_neg1_inclusive_and_mean_exlcusive = "between_neg1_inclusive_and_mean_exlcusive"
  val between_mean_inclusive_and_pos1_exlcusive = "between_mean_inclusive_and_pos1_exlcusive"
  val between_pos1_inclusive_and_pos2_exlcusive = "between_pos1_inclusive_and_pos2_exlcusive"
  val between_pos2_inclusive_and_pos3_exlcusive = "between_pos2_inclusive_and_pos3_exlcusive"
  val above_pos3_sdev = "above_pos3_sdev"

  val gaussianDiscretizedValueBases = Seq(
    below_neg3_sdev,
    between_neg3_inclusive_and_neg2_exclusive,
    between_neg2_inclusive_and_neg1_exclusive,
    between_neg1_inclusive_and_mean_exlcusive,
    between_mean_inclusive_and_pos1_exlcusive,
    between_pos1_inclusive_and_pos2_exlcusive,
    between_pos2_inclusive_and_pos3_exlcusive,
    above_pos3_sdev
  )

  def gaussianRule[N: Numeric](g: Gaussian[N]): Rule[N] = new Rule[N] {

    override def apply(value: N): String = {
      /*
        if value < g.mean - 3 *g.stddev then below_neg_3_sdev
        ...
        else above_pos_3_sdev
       */

      ???
    }

    override val discretizedValueBases = gaussianDiscretizedValueBases
  }
}