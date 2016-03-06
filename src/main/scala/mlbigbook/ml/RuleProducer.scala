package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ NumericConversion, MathVectorOps }

import scala.language.higherKinds
import scala.reflect.ClassTag

abstract class RuleProducer[N: ClassTag: NumericConversion] {

  def apply[D[_]: Data, V[_]](data: D[V[N]])(
    implicit
    fs:   FeatureSpace,
    vops: MathVectorOps[N, V]
  ): Seq[Rule[N]]

}