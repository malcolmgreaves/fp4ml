package mlbigbook.ml

import breeze.linalg.DenseVector
import mlbigbook.math.VectorOpsT
import org.scalatest.FunSuite

class BinaryDiscretizationTest extends FunSuite {

  import fif.ImplicitCollectionsData._
  import VectorOpsT.Implicits._

  test("no rules when called on empty data") {
    val data = Seq.empty[(DenseVector[Double], Boolean)]
    implicit val _ = FeatureSpace.empty
    assert(BinaryDiscretization(data) === Seq.empty[Rule[Double]])
  }

  test("properly discretizes one-feature vectors") {
    val data = Seq((DenseVector(5.0), false))
    implicit val fs = RealFeatureSpace(Seq("feature"))
    val rules = BinaryDiscretization(data)
    assert(rules.size === fs.size)
    val rule = rules.head

    val input = Seq(
      (DenseVector(5.0), false),
      (DenseVector(4.0), true),
      (DenseVector(10.0), false)
    )

    val expected = Seq(
      BinaryDiscretization.above_or_equal,
      BinaryDiscretization.below,
      BinaryDiscretization.above_or_equal
    )

    val actual = input.map { case (v, _) => rule(v(0)) }

    actual.zip(expected)
      .foreach { case (a, e) => assert(a === e) }
  }

}