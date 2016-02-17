package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object MdlDiscretization {

  def apply[D[_]: Data, V[_], N: Numeric: MathOps: ClassTag](
    data: D[(V[N], Boolean)]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): Seq[Rule[N]] =
    Mdl(data).map { mdlRule[N] }

  def mdlRule[N: Numeric](ts: Mdl.Thresholds[N]): Rule[N] =
    if (ts isEmpty)
      new Rule[N] {
        override def apply(ignore: N) = ""
        override val discretizedValueBases = Seq("")
      }
    else
      Rule(
        ts.map { threshold => (threshold, s"below_$threshold") },
        s"at_or_above_maximum_threshold_${ts.last}"
      )

  def ruleProducer[N: NumericConversion: MathOps: ClassTag]: SupervisedRuleProducer[N] =
    new SupervisedRuleProducer[N] {
      implicit val _ = NumericConversion[N].numeric

      override def apply[D[_]: Data, V[_]](data: D[(V[N], Boolean)])(
        implicit
        fs:   FeatureSpace,
        vops: VectorOpsT[N, V]
      ): Seq[Rule[N]] =
        MdlDiscretization(data)
    }

}