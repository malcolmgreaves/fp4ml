package mlbigbook.ml

import fif.Data
import Data.ops._
import mlbigbook.math.{ NumericConversion, MathVectorOps }

import scala.language.higherKinds
import scala.reflect.ClassTag

object BinaryDiscretization {

  val below = "below"
  val above_or_equal = "above_or_equal"

  val binaryDiscreteValueBases = Seq(below, above_or_equal)

  def apply[D[_]: Data, V[_], N: Numeric: ClassTag](
    data: D[(V[N], Boolean)]
  )(
    implicit
    vops: MathVectorOps[N, V],
    fs:   FeatureSpace
  ): Seq[Rule[N]] =
    if (data isEmpty)
      Seq.empty[Rule[N]]
    else
      CutPoint(data).map(cutPointRule[N])

  def cutPointRule[N: Numeric](threshold: N): Rule[N] =
    Rule(
      Seq((threshold, below)),
      above_or_equal
    )

  def ruleProducer[N: NumericConversion: MathOps: ClassTag]: SupervisedRuleProducer[N] =
    new SupervisedRuleProducer[N] {

      implicit val num = NumericConversion[N].numeric

      override def apply[D[_]: Data, V[_]](data: D[(V[N], Boolean)])(
        implicit
        fs:   FeatureSpace,
        vops: MathVectorOps[N, V]
      ): Seq[Rule[N]] =
        BinaryDiscretization(data)
    }

}