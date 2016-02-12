package mlbigbook.ml

import breeze.linalg.Vector
import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }

import scala.language.higherKinds
import scala.reflect.ClassTag

trait RuleProducer {

  def apply[D[_]: Data, V[_] <: Vector[_], N: NumericConversion: MathOps: ClassTag](
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