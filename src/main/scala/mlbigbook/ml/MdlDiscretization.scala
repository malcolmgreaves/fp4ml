package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object MdlDiscretization {

  def apply[D[_]: Data, V[_], N: NumericConversion: MathOps](
    data: D[(V[N], Boolean)]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): Seq[Rule[N]] = {

    ???
  }

  def ruleProducer[N: NumericConversion: MathOps: ClassTag]: SupervisedRuleProducer[N] =
    new SupervisedRuleProducer[N] {
      override def apply[D[_]: Data, V[_]](data: D[(V[N], Boolean)])(
        implicit
        fs:   FeatureSpace,
        vops: VectorOpsT[N, V]
      ): Seq[Rule[N]] =
        MdlDiscretization(data)
    }

}