//package mlbigbook.ml
//
//import fif.Data
//import Data.ops._
//import mlbigbook.math.{ NumericConversion, VectorOpsT }
//
//import scala.language.higherKinds
//import scala.reflect.ClassTag
//
//object BinaryDiscretization {
//
//  val below = "below"
//  val above_or_equal = "above_or_equal"
//
//  val binaryDiscreteValueBases = Seq(below, above_or_equal)
//
//  def apply[D[_]: Data, V[_], N: NumericConversion: MathOps](
//    data: D[(V[N], Boolean)]
//  )(
//    implicit
//    vops: VectorOpsT[N, V],
//    fs:   FeatureSpace
//  ): Seq[Rule[N]] =
//    if (data isEmpty)
//      Seq.empty[Rule[N]]
//    else {
//      CutPoint(data).map(cutPointRule)
//    }
//
//  def cutPointRule[N: Numeric](threshold: N): Rule[N] =
//    Rule(Seq(threshold, below), above_or_equal)
//
//  def ruleProducer[N: NumericConversion: MathOps: ClassTag]: SupervisedRuleProducer[N] =
//    new SupervisedRuleProducer[N] {
//      override def apply[D[_]: Data, V[_]](data: D[(V[N], Boolean)])(
//        implicit
//        fs:   FeatureSpace,
//        vops: VectorOpsT[N, V]
//      ): Seq[Rule[N]] =
//        BinaryDiscretization(data)
//    }
//
//}