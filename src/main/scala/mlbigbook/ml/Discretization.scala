package mlbigbook.ml

import fif.Data
import mlbigbook.math.{ NumericConversion, VectorOpsT }
import spire.syntax.cfor._

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.ClassTag

object Discretization {

  import Data.ops._

  type DiscretizedVector = Seq[String]

  //
  // UNLABELED DATA
  //

  def apply[D[_]: Data, V[_], N: NumericConversion: ClassTag](
    data: D[V[N]],
    rp:   RuleProducer[N]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): (D[DiscretizedVector], CategoricalFeatureSpace) =
    if (data isEmpty)
      (data.map(_ => Seq.empty[String]), CategoricalFeatureSpace.empty)
    else
      unlabeled(data, rp(data))

  def unlabeled[D[_]: Data, V[_], N: NumericConversion: ClassTag](
    data:         D[V[N]],
    readyRulings: Seq[Rule[N]]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): (D[DiscretizedVector], CategoricalFeatureSpace) =

    if (data isEmpty)
      (
        data.map { _ => Seq.empty[String] },
        CategoricalFeatureSpace.empty
      )

    else {
      val discretizer = discretizeVector(readyRulings)
      implicit val _ = NumericConversion[N].numeric
      (
        data.map { discretizer },
        Discretization.newCategoricalFs(readyRulings)
      )
    }

  //
  // LABELED DATA
  //

  def apply[D[_]: Data, V[_], N: NumericConversion: ClassTag](
    data: D[(V[N], Boolean)],
    rp:   SupervisedRuleProducer[N]
  )(
    implicit
    vops:     VectorOpsT[N, V],
    fs:       FeatureSpace,
    ctNumVec: ClassTag[V[N]]
  ): (D[(DiscretizedVector, Boolean)], CategoricalFeatureSpace) =
    if (data isEmpty)
      (
        data.map { case (_, label) => (Seq.empty[String], label) },
        CategoricalFeatureSpace.empty
      )
    else
      labeled(data, rp(data))

  def labeled[D[_]: Data, V[_], N: NumericConversion: ClassTag](
    data:         D[(V[N], Boolean)],
    readyRulings: Seq[Rule[N]]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): (D[(DiscretizedVector, Boolean)], CategoricalFeatureSpace) =

    if (data isEmpty)
      (
        data.map { case (_, label) => (Seq.empty[String], label) },
        CategoricalFeatureSpace.empty
      )

    else {

      val discretizer = discretizeVector(readyRulings)
      implicit val _ = NumericConversion[N].numeric
      (
        data.map { case (vector, label) => (discretizer(vector), label) },
        Discretization.newCategoricalFs(readyRulings)
      )
    }

  //
  // MISC
  //

  def discretizeVector[N: NumericConversion, V[_]](
    readyRulings: Seq[Rule[N]]
  )(
    implicit
    vops: VectorOpsT[N, V],
    fs:   FeatureSpace
  ): V[N] => DiscretizedVector =
    vector => {
      val valAt = vops.valueAt(vector) _
      //
      // MUTATION WARNING
      //
      // We allocate a local array and loop over it to apply the ruling for
      // each feature. Outside this function, we cannot view this side
      // effect as we evaluate to a Seq[String] (converting the Array we're
      // building).
      //
      val res = new Array[String](fs.size)
      cfor(0)(_ < fs.size, _ + 1) { fIndex =>
        res(fIndex) = readyRulings(fIndex)(valAt(fIndex))
      }
      res.toSeq
      // equivalent to the following side-effect free code:
      // VectorOpsT[N,V].toSeq(vector).zip(readyRulings)
      //   .map { case (value, rule) => rule(value) }
    }

  def newCategoricalFs(
    rules: Seq[Rule[_]]
  )(implicit fs: FeatureSpace): CategoricalFeatureSpace =
    CategoricalFeatureSpace(
      fs.features,
      fs.features.zip(rules)
      .map {
        case (feature, rule) =>
          (
            feature,
            rule.discretizedValueBases.map { dv => s"$dv--$feature" }
          )
      }
      .toMap
    )

}