package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.higherKinds
import scala.reflect.ClassTag

abstract class SupervisedRuleProducer[N: ClassTag: NumericConversion] {

  def apply[D[_]: Data, V[_]](data: D[(V[N], Boolean)])(
    implicit
    fs:   FeatureSpace,
    vops: VectorOpsT[N, V]
  ): Seq[Rule[N]]

}