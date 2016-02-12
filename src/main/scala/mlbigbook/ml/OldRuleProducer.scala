package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.higherKinds
import scala.reflect.ClassTag

trait RuleProducer {

  /** Number type */
  type N

  implicit def ct: ClassTag[N]
  implicit def numConv: NumericConversion[N]
  implicit lazy val num: Numeric[N] = numConv.numeric

  //  implicit def vops: VectorOpsT[N, V]

  def apply[D[_]: Data, V[_]](data: D[V[N]])(
    implicit
    fs:   FeatureSpace,
    vops: VectorOpsT[N, V]
  ): Seq[Rule[N]]

}

object RuleProducer {

  type Type[Num] = RuleProducer { type N = Num }
}

abstract class Rule[N: Numeric] {

  def apply(value: N): String

  def discretizedValueBases: Seq[String]
}