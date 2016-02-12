package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.higherKinds
import scala.reflect.ClassTag

trait RuleProducer {

  /** Number type */
  type N
  /** Vector type */
  type V[_]

  implicit def ct: ClassTag[N]
  implicit def numConv: NumericConversion[N]
  implicit lazy val num: Numeric[N] = numConv.numeric

  implicit def vops: VectorOpsT[N, V]

  def apply[D[_]: Data](data: D[V[N]])(implicit fs: FeatureSpace): Seq[Rule[N]]

}

object RuleProducer {

  type Type[Num, Vec[_]] = RuleProducer {
    type N = Num
    type V[_] = Vec[_]
  }

}

trait OldRuleProducer {

  def apply[D[_]: Data, V[_], N: NumericConversion: ClassTag](
    data: D[V[N]]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): Seq[Rule[N]]

}

abstract class Rule[N: Numeric] {

  def apply(value: N): String

  def discretizedValueBases: Seq[String]
}