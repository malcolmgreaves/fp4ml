package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.higherKinds
import scala.reflect.ClassTag

object BinaryDiscretization {

  val below = "below"
  val above_or_equal = "above_or_equal"

  val binaryDiscreteValueBases = Seq(below, above_or_equal)

  def apply[D[_]: Data, V[_], N: NumericConversion: MathOps](data: D[V[N]])(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): Seq[Rule[N]] = ???

  def cutPoint[N: Numeric](threshold: N): Rule[N] = new Rule[N] {

    val lessThan = implicitly[Numeric[N]].lt _

    override def apply(value: N): String =
      if (lessThan(value, threshold)) below
      else above_or_equal

    override val discretizedValueBases = binaryDiscreteValueBases
  }
  /*
    Rule(Seq(threshold, below), above_or_equal)
   */

  def ruleProducer[N: NumericConversion: MathOps: ClassTag]: RuleProducer[N] =
    new RuleProducer[N] {
      override def apply[D[_]: Data, V[_]](data: D[V[N]])(
        implicit
        fs:   FeatureSpace,
        vops: VectorOpsT[N, V]
      ): Seq[Rule[N]] =
        BinaryDiscretization(data)
    }

}